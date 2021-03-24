;;; elpa-mirror.el --- Create local package repository from installed packages

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/elpa-mirror
;; Package-Requires: ((emacs "25.1"))
;; Version: 2.1.5
;; Keywords: tools
;;
;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of elpa-mirror
;;
;; elpa-mirror is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; elpa-mirror is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program will create a local package repository by from all
;; installed packages.
;;
;; Please note compile Emacs Lisp file (*.elc) from one version of Emacs
;; might not work with another version of Emacs.  So you need this program
;; to compile package from local repository.
;;
;; This is the ONLY way to have 100% portable Emacs setup.
;;
;; Usage in Emacs,
;; Run `elpamr-create-mirror-for-installed'.
;;
;; CLI program tar is required.  It's already installed on Windows10/Linux/macOS.
;;
;; Usage in Shell,
;;   Emacs --batch -l ~/.emacs.d/init.el
;;         -l ~/any-directory-you-prefer/elpa-mirror.el \
;;         --eval='(setq elpamr-default-output-directory "~/myelpa")' \
;;         --eval='(elpamr-create-mirror-for-installed)
;;
;; Use the repository created by elpa-mirror,
;;   - Insert `(setq package-archives '(("myelpa" . "~/myelpa/")))` into ~/.emacs
;;   - Restart Emacs
;;
;; Tips,
;;   - `elpamr-exclude-packages' exclude packages
;;   - `elpamr-tar-command-exclude-patterns' excludes file and directories in
;;   package directory.
;;   - You can also setup repositories on Dropbox and Github.
;;   See https://github.com/redguardtoo/elpa-mirror for details.

;;; Code:
(require 'package)

(defcustom elpamr-default-output-directory nil
  "The output directory use by `elpamr-create-mirror-for-installed'."
  :type '(choice directory (const :tags "None" nil))
  :group 'elpa-mirror)

(defcustom elpamr-exclude-packages nil
  "Names of excluded packages."
  :type '(repeat string)
  :group 'elpa-mirror)

(defcustom elpamr-tar-command-exclude-patterns
  '("*.elc"
    "*~"
    "*.so"
    "*.dylib"
    "*.dll"
    "*/bin"
    "*/__pycache__")
  "Exclude patterns passed tar's `--exclude' option.

The patterns use shell glob syntax, not regexp syntax:

* `*' matches any string, including `/'.
* `?' matches a single character.
* `[abc]' or `[a-z]' is a character class.
* `[^a-z]' or `[!a-z]' is a negated character class.
* `^' and `$' have a special meaning in BSD tar only.
* Special characters are quoted with `\\'.

The patterns are anchored, meaning that they always start
matching at the start of the path. This is done by passing the
`--anchored' option when running with GNU tar, or prepending `^'
to every pattern when running with BSD tar.

Examples:

* Exclude files/directories that end with `.elc': `*.elc'.
* Exclude files/directories named `__pycache__': `*/__pycache__'.
* Exclude `bin' inside the `company' package: `company-*/bin'.

Note that a slash at the start or the end of a pattern will cause
it to match nothing."
  :type '(repeat string)
  :group 'elpa-mirror)

(defcustom elpamr-tar-executable
  "tar"
  "The tar executable used by elpa-mirror.
It can be BSD tar, but GNU tar is preferred."
  :type 'string
  :group 'elpa-mirror)

(defcustom elpamr-finished-hook nil
  "Hook run when command `elpamr-create-mirror-for-installed' run finished.
The hook function have one argument: output-directory."
  :group 'elpa-mirror
  :type 'hook)

(defvar elpamr--log-buffer "*elpa-mirror log*"
  "Destination buffer for log messages and command output.")

(defun elpamr--log (format-string &rest args)
  "Format ARGS with FORMAT-STRING, add the result to the log, and return it.
The log line will be prepended with an asterisk to distinguish it
from program output."
  (let ((line (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create elpamr--log-buffer)
      (insert "* " line "\n"))
    line))

(defun elpamr--log-message (format-string &rest args)
  "Format ARGS with FORMAT-STRING, add the result to the log, and
display it."
  (apply #'elpamr--log format-string args)
  (apply #'message format-string args))

(defun elpamr--log-error (format-string &rest args)
  "Format ARGS with FORMAT-STRING, add the result to the log, and
signal an error."
  (apply #'elpamr--log format-string args)
  (apply #'error format-string args))

(defun elpamr--package-desc (item)
  "Extract package information from ITEM."
  (cadr item))

(defun elpamr--is-bsd-tar ()
  "Are we using BSD tar instead of GNU tar?"
  (let* ((output (mapconcat #'identity (process-lines elpamr-tar-executable "--version") " "))
         (result (and output (string-match-p "^[ \t]*bsdtar" output))))
    (elpamr--log "Detected tar variant: %s" (if result "BSD" "GNU"))
    result))

(defun elpamr--create-one-item-for-archive-contents (pkg)
  "Access PKG extracted from `package-alist' directly."
  (unless (member (symbol-name (car pkg)) elpamr-exclude-packages)
    pkg))

(defun elpamr--extract-info-from-dir (dirname)
  "Extract information from DIRNAME.
Return `(list package-name integer-version-number)' or nil."
  (interactive)
  (when (string-match "\\(.*\\)-\\([0-9.]+\\)$" dirname)
    (list (match-string 1 dirname)
          (split-string (match-string 2 dirname) "\\."))))

(defun elpamr--fullpath (parent file)
  "Full path of 'PARENT/FILE'."
  (let* ((result (file-truename (concat (file-name-as-directory parent) file))))
    (elpamr--log "Converted to full path: %S %S -> %S" parent file result)
    result))

(defun elpamr--clean-package-description (description)
  "Clean DESCRIPTION."
  (replace-regexp-in-string "-\*-.*-\*-" ""
                            (replace-regexp-in-string "\"" "" description t)
                            t))

(defun elpamr--get-dependency (item)
  "Get ITEM dependency."
  (package-desc-reqs (elpamr--package-desc item)))

(defun elpamr--get-version (item)
  "Get ITEM version."
  (package-desc-version (elpamr--package-desc item)))

(defun elpamr--get-summary (item)
  "Get ITEM description."
  (package-desc-summary (elpamr--package-desc item)))

(defun elpamr--one-item-for-archive-contents (final-pkg)
  "Format FINAL-PKG information into a string for archive-contents."
  (format " (%s . [%S %S \"%s\" tar])\n"
          (car final-pkg)
          (elpamr--get-version final-pkg)
          (elpamr--get-dependency final-pkg)
          (elpamr--clean-package-description (elpamr--get-summary final-pkg))))

(defun elpamr--call-process-check (program &rest call-process-args)
  "Call `call-process' with the arguments to this function.
Log and signal an error if it exits with a non-zero status."
  (let ((exit-status (apply #'call-process program call-process-args)))
    (if (not (= exit-status 0))
        (elpamr--log-error
         "Program %s exited with non-zero status %s, see the %s buffer for details"
         program exit-status elpamr--log-buffer)
      exit-status)))

(defun elpamr--run-tar (working-dir out-file dir-to-archive is-bsd-tar)
  "Run tar in order to archive DIR-TO-ARCHIVE into OUT-FILE.
Paths are relative to WORKING-DIR.
IS-BSD-TAR should be non-nil if this function should use a
command compatible with BSD tar instead of GNU tar."
  ;; We could detect BSD tar inside this function easily, but detecting it once
  ;; and then passing it as an argument improves performance.
  (let* ((exclude-opts (mapcar (lambda (s)
                                 (concat "--exclude=" (if is-bsd-tar "^" "") s))
                               elpamr-tar-command-exclude-patterns))
         ;; create tar using GNU tar
         (tar-args
          `("cf" ,out-file
            ,@(unless is-bsd-tar '("--anchored"))
            ,@exclude-opts
            ;; tar 1.14 NEWS,
            ;; @see https://git.savannah.gnu.org/cgit/tar.git/plain/NEWS?id=release_1_14
            ;; * New option --format allows to select the output archive format
            ;; * The default output format can be selected at configuration time
            ;;   by presetting the environment variable DEFAULT_ARCHIVE_FORMAT.
            ;;   Allowed values are GNU, V7, OLDGNU and POSIX.
            ,@(unless is-bsd-tar '("--format=gnu"))
            ;; Improve reproducibility by not storing unnecessary metadata.
            ;; These options are enough for archives in the GNU format, but if
            ;; we ever switch to PAX, we'll need to add more (see
            ;; <http://h2.jaguarpaw.co.uk/posts/reproducible-tar/> and
            ;; <https://www.gnu.org/software/tar/manual/html_node/PAX-keywords.html>).
            ,@(unless is-bsd-tar
                '("--sort=name"
                  "--owner=root:0" "--group=root:0"
                  "--mtime=1970-01-01 00:00:00 UTC"))
            ;; It's important that tar starts in the package's parent directory
            ;; (using the `-C' option) and gets passed just the name (not the
            ;; full path) of the package's directory. This is because we want
            ;; anchored exclude patterns like `company-*/bin' to do what the
            ;; user expects.
            "-C" ,working-dir
            "--" ,dir-to-archive))
         ;; Don't archive macOS' file properties (see
         ;; <https://superuser.com/q/259703>).
         (process-environment (if (eq system-type 'darwin)
                                  (cons "COPYFILE_DISABLE=" process-environment)
                                process-environment)))
    (elpamr--log "Running tar: %S %S" elpamr-tar-executable tar-args)
    (apply #'elpamr--call-process-check
           elpamr-tar-executable nil
           elpamr--log-buffer nil
           tar-args)))

;;;###autoload
(defun elpamr-version ()
  "Current version."
  (interactive)
  (message "2.1.5"))

;;;###autoload
(defun elpamr-create-mirror-for-installed (&optional output-directory recreate-directory)
  "Export installed packages into a new directory.
Create the html files for the mirror site.

The first valid directory found from the below list
will be used as mirror package's output directory:
1. Argument: OUTPUT-DIRECTORY
2. Variable: `elpamr-default-output-directory'
3. Ask user to provide.

When RECREATE-DIRECTORY is non-nil, OUTPUT-DIRECTORY
will be deleted and recreated."
  (interactive)
  (let (final-pkg-list)

    ;; Erase the log (in case of multiple consecutive calls to this function).
    (with-current-buffer (get-buffer-create elpamr--log-buffer)
      (erase-buffer))

    ;; Since Emacs 27, `package-initialize' is optional.
    ;; but we still need it to initialize `package-alist'.
    (unless package-alist (package-initialize))

    ;; Quote from manual about package-alist:
    ;;   Alist of all packages available for activation.
    ;;   Each element has the form (PKG . DESCS), where PKG is a package
    ;;   name (a symbol) and DESCS is a non-empty list of `package-desc' structure,
    ;;   sorted by decreasing versions.
    ;; Sorted for reproducibility.
    (setq final-pkg-list
          (let ((sorted-package-alist
                 (sort (copy-sequence package-alist)
                       (lambda (a b)
                         (string< (symbol-name (car a))
                                  (symbol-name (car b)))))))
            (delq nil (mapcar #'elpamr--create-one-item-for-archive-contents
                              sorted-package-alist))))

    ;; set output directory
    (setq output-directory
          (cond ((and output-directory
                      (stringp output-directory))
                 (file-name-as-directory output-directory))
                ((and elpamr-default-output-directory
                      (stringp elpamr-default-output-directory))
                 (file-name-as-directory elpamr-default-output-directory))
                (t (read-directory-name "Output directory: "))))

    ;; Delete output directory if we need a clean output directory
    (when (and recreate-directory
               (file-directory-p output-directory))
      (elpamr--log-message "Re-creating %s" output-directory)
      (delete-directory output-directory t))

    ;; Create output directory if it does not exist.
    (unless (file-directory-p output-directory)
      (make-directory output-directory t))

    (when (and (> (length final-pkg-list) 0)
               output-directory
               (file-directory-p output-directory))

      (let* ((pkg-dir (file-truename package-user-dir))
             (dirs (directory-files pkg-dir))
             (is-bsd-tar (elpamr--is-bsd-tar))
             (cnt 0))
        (dolist (dir dirs)
          (unless (or (member dir '("archives" "." ".."))
                      (not (elpamr--extract-info-from-dir dir)))
            (elpamr--run-tar pkg-dir
                             (concat (elpamr--fullpath output-directory dir) ".tar")
                             dir
                             is-bsd-tar)
            (setq cnt (1+ cnt))
            (message "Creating *.tar... %2d%% (%s)"
                     (/ (* cnt 100) (length dirs))
                     dir))))

      ;; output archive-contents
      (elpamr--log-message "Creating archive-contents...")
      (with-temp-buffer
        (let* ((print-level nil)
               (print-length nil))
          (insert "(1\n")
          (dolist (final-pkg final-pkg-list)
            ;; each package occupies one line
            (insert (elpamr--one-item-for-archive-contents final-pkg)))
          (insert ")"))
        (write-file (elpamr--fullpath output-directory "archive-contents")))
      (run-hook-with-args 'elpamr-finished-hook output-directory)
      (elpamr--log-message "DONE! Output directory: %s" output-directory))))

(provide 'elpa-mirror)
;;; elpa-mirror.el ends here
