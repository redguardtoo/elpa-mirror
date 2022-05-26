;;; elpa-mirror.el --- Create local package repository from installed packages

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/elpa-mirror
;; Package-Requires: ((emacs "25.1"))
;; Version: 2.2.0
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
;; CLI program tar is required.  It's bundled with Windows10/Linux/macOS.
;;
;; Usage in Shell,
;;   Emacs --batch -l ~/.emacs.d/init.el
;;         -l ~/any-directory-you-prefer/elpa-mirror.el \
;;         --eval='(setq elpamr-default-output-directory "~/myelpa")' \
;;         --eval='(elpamr-create-mirror-for-installed)
;;
;; Use the repository created by elpa-mirror,
;;   - Add `(setq package-archives '(("myelpa" . "~/myelpa/")))` into ~/.emacs
;;   - Restart Emacs
;;
;; Tips,
;;   - `elpamr-exclude-packages' excludes packages
;;
;;   - `elpamr-tar-command-exclude-patterns' excludes file and directories in
;;   package directory.
;;
;;   - `elpamr-exclude-patterns-filter-function' lets users define a function to
;;     exclude files and directories per package.
;;
;;     Below setup adds directory "bin/" into package "vagrant-tramp".
;;
;;     (setq elpamr-exclude-patterns-filter-function
;;           (lambda (package-dir)
;;             (let ((patterns elpamr-tar-command-exclude-patterns))
;;               (when (string-match "vagrant-tramp" package-dir)
;;                 (setq patterns (remove "*/bin" patterns)))
;;               patterns)))

;;   - You can also setup repositories on Dropbox and Github.
;;   See https://github.com/redguardtoo/elpa-mirror for details.
;;

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
matching at the start of the path.  This is done by passing the
`--anchored' option when running with GNU tar, or pre-pending `^'
to every pattern when running with BSD tar.

Examples:

* Exclude files/directories that end with `.elc': `*.elc'.
* Exclude files/directories named `__pycache__': `*/__pycache__'.
* Exclude `bin' inside the `company' package: `company-*/bin'.

Note that a slash at the start or the end of a pattern will cause
it to match nothing."
  :type '(repeat string)
  :group 'elpa-mirror)

(defcustom elpamr-exclude-patterns-filter-function nil
  "Filter `elpamr-tar-command-exclude-patterns' before using it per package.
A function with one parameter which is the package directory.
It returns the result to replace `elpamr-tar-command-exclude-patterns'."
  :group 'elpa-mirror
  :type 'hook)

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

(defcustom elpamr-enable-log nil
  "Enable log."
  :type 'boolean
  :group 'elpa-mirror)

(defvar elpamr--log-buffer "*elpa-mirror log*"
  "Destination buffer for log messages and command output.")

(defun elpamr--log (format-string &rest args)
  "Format ARGS with FORMAT-STRING, add the result to the log, and return it.
The log line will be pre-pended with an asterisk to distinguish it
from program output."
  (when elpamr-enable-log
    (let ((line (apply #'format format-string args)))
      (with-current-buffer (get-buffer-create elpamr--log-buffer)
        (insert "* " line "\n"))
      line)))

(defun elpamr--log-message (format-string &rest args)
  "Format ARGS with FORMAT-STRING, add the result to the log and display it."
  (when elpamr-enable-log
    (apply #'elpamr--log format-string args)
    (apply #'message format-string args)))

(defun elpamr--log-error (format-string &rest args)
  "Format ARGS with FORMAT-STRING, add the result to the log and signal an error."
  (when elpamr-enable-log
(apply #'elpamr--log format-string args)
  (apply #'error format-string args)))

(defun elpamr--package-desc (item)
  "Extract package information from ITEM."
  (cadr item))

(defun elpamr--is-bsd-tar ()
  "Are we using BSD tar instead of GNU tar?"
  (let* ((output (mapconcat #'identity (process-lines elpamr-tar-executable "--version") " "))
         ;; @see https://github.com/redguardtoo/elpa-mirror/issues/37
         ;; extra error message insert extra whitespace before "bsdtar"
         (result (and output (string-match-p "\\(^[ \t]*\\|[ \t]\\)bsdtar" output))))
    (elpamr--log "Detected tar variant: %s" (if result "BSD" "GNU"))
    result))

(defun elpamr--create-one-item-for-archive-contents (pkg)
  "Access PKG extracted from `package-alist' directly."
  (unless (member (symbol-name (car pkg)) elpamr-exclude-packages)
    pkg))

(defun elpamr--extract-info-from-dir (dirname)
  "Extract information from DIRNAME.
Return `(list package-name integer-version-number)' or nil."
  (when (string-match "\\(.*\\)-\\([0-9][0-9a-z.]+\\)$" dirname)
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

(defun elpamr--call-process-check (arguments)
  "Call run tar program with the ARGUMENTS.
Log and signal an error if it exits with a non-zero status."
  (let ((exit-status (apply #'call-process
                            elpamr-tar-executable
                            nil
                            (and elpamr-enable-log elpamr--log-buffer)
                            nil
                            arguments)))
    (cond
     ((not (= exit-status 0))
      (elpamr--log-error
       "Program %s exited with non-zero status %s, see the %s buffer for details"
       elpamr-tar-executable exit-status elpamr--log-buffer)
      )
     (t
      exit-status))))

(defun elpamr--run-tar (working-dir out-file dir-to-archive is-bsd-tar)
  "Run tar in order to archive DIR-TO-ARCHIVE into OUT-FILE.
Paths are relative to WORKING-DIR.
IS-BSD-TAR should be non-nil if this function should use a
command compatible with BSD tar instead of GNU tar."
  ;; We could detect BSD tar inside this function easily, but detecting it once
  ;; and then passing it as an argument improves performance.
  (let* ((exclude-opts (mapcar (lambda (s)
                                 (concat "--exclude=" (if is-bsd-tar "^" "") s))
                               (if elpamr-exclude-patterns-filter-function
                                   (funcall elpamr-exclude-patterns-filter-function
                                            dir-to-archive)
                                 elpamr-tar-command-exclude-patterns)))
         ;; set pwd of process
         (default-directory working-dir)
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
                ;; @see https://github.com/redguardtoo/elpa-mirror/issues/41
                '("--owner=root"
                  "--group=root"
                  "--mtime=1970-01-01 00:00:00 UTC"))
            "--" ,dir-to-archive))
         ;; Don't archive macOS' file properties (see
         ;; <https://superuser.com/q/259703>).
         (process-environment (if (eq system-type 'darwin)
                                  (cons "COPYFILE_DISABLE=" process-environment)
                                process-environment)))
    (elpamr--log "Running tar: %S %S" elpamr-tar-executable tar-args)
    (elpamr--call-process-check tar-args)))

;;;###autoload
(defun elpamr-version ()
  "Current version."
  (interactive)
  (message "2.2.0"))

(defun elpamr--win-executable-find (exe)
  "Find EXE on windows."
  (let* ((drivers '("c" "d" "e" "f"))
          (i 0)
          j
          (dirs '(":\\\\cygwin64\\\\bin\\\\"
                 ":\\\\msys64\\\\usr\\\\bin\\\\"))
          rlt)
     (while (and (not rlt)
                 (< i (length dirs)))
       (setq j 0)
       (while (and (not rlt)
                   (< j (length drivers)))
         (setq rlt (executable-find (concat (nth j drivers) (nth i dirs) exe)))
         (setq j (1+ j)))
       (setq i (1+ i)))
     (unless rlt
       ;; nothing found, fall back to exe
       (setq rlt exe))
     rlt))

(defun elpamr-double-check-executable ()
  "Make sure `elpamr-tar-executable' is executable."
  (when (and (not (file-executable-p elpamr-tar-executable))
             (eq system-type 'windows-nt))
    (setq elpamr-tar-executable (elpamr--win-executable-find elpamr-tar-executable))))

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

  ;; find tar program on Windows if GNU tar from Cygwin/MYSYS2 is installed
  ;; and current `elpamr-tar-executable' is NOT executable.
  (elpamr-double-check-executable)

  (let (final-pkg-list)

    ;; Erase the log (in case of multiple consecutive calls to this function).
    (when elpamr-enable-log
      (with-current-buffer (get-buffer-create elpamr--log-buffer)
        (erase-buffer)))

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
                             ;; use relative path in case we use cygwin tar
                             (file-relative-name (concat (elpamr--fullpath output-directory dir) ".tar") pkg-dir)
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
