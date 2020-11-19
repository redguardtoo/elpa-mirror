;;; elpa-mirror.el --- Create local package repository from installed packages

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/elpa-mirror
;; Package-Requires: ((emacs "24.4"))
;; Version: 2.1.2
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
;; CLI program tar is required.  It's already installed on Windows10/Linux/macOS.
;; On old window, the easiest way to get tar is installing Cygwin/MSYS2.
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
;; You can also setup repositories on Dropbox and Github.
;; See https://github.com/redguardtoo/elpa-mirror for details.

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

(defcustom elpamr-finished-hook nil
  "Hook run when command `elpamr-create-mirror-for-installed' run finished.
The hook function have one argument: output-directory."
  :group 'elpa-mirror
  :type 'hook)

(defvar elpamr-debug nil "Show debug message.")

(defun elpamr--package-desc (item)
  "Extract package information from ITEM."
  (cadr item))

(defun elpamr--is-mac ()
  "Is macOS?"
  (eq system-type 'darwin))

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

(defun elpamr--win-executable-find (driver path exe)
  "GNU Find executable with DRIVER/PATH/EXE information provided."
  (when (executable-find (concat driver path exe))
    (concat driver path exe)))

(defun elpamr--executable-find (exe)
  "GNU Find EXE on Windows."
  (or (and (eq system-type 'windows-nt)
           (or
            ;; cygwin
            (elpamr--win-executable-find "c" ":\\\\cygwin64\\\\bin\\\\" exe)
            (elpamr--win-executable-find "d" ":\\\\cygwin64\\\\bin\\\\" exe)
            (elpamr--win-executable-find "e" ":\\\\cygwin64\\\\bin\\\\" exe)
            (elpamr--win-executable-find "f" ":\\\\cygwin64\\\\bin\\\\" exe)
            ;; msys2
            (elpamr--win-executable-find "c" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)
            (elpamr--win-executable-find "d" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)
            (elpamr--win-executable-find "e" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)
            (elpamr--win-executable-find "f" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)))
      ;; *nix
      (executable-find exe)
      ;; well, `executable-find' failed
      exe))

(defun elpamr--fullpath (parent file &optional no-convertion)
  "Full path of 'PARENT/FILE'.
If NO-CONVERTION is t,  it's UNIX path."
  (let* ((rlt (file-truename (concat (file-name-as-directory parent) file)))
         cmd)
    (when (and (eq system-type 'windows-nt) (not no-convertion))
      (setq cmd (format "%s -u \"%s\""
                            (elpamr--executable-find "cygpath")
                            rlt))
      (setq rlt (replace-regexp-in-string "[\r\n]+"
                                          ""
                                          (shell-command-to-string cmd))))
    (if elpamr-debug (message "elpamr--fullpath called => %s" rlt))
    rlt))

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

;;;###autoload
(defun elpamr-version ()
  "Current version."
  (interactive)
  (message "2.1.2"))

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
  (let* (item
         final-pkg-list
         tar-cmd
         ;; package-user-dir is ~/.emacs.d/elpa by default
         (dirs (directory-files package-user-dir))
         (cnt 0))
    ;; quoted from manual:
    ;;   Alist of all packages available for activation.
    ;;   Each element has the form (PKG . DESCS), where PKG is a package
    ;;   name (a symbol) and DESCS is a non-empty list of `package-desc' structure,
    ;;   sorted by decreasing versions.
    (dolist (pkg package-alist)
      (setq item (elpamr--create-one-item-for-archive-contents pkg))
      (if item (push item final-pkg-list)))

    ;; set output directory
    (setq output-directory
          (cond ((and output-directory
                      (stringp output-directory))
                 (file-name-as-directory output-directory))
                ((and elpamr-default-output-directory
                      (stringp elpamr-default-output-directory))
                 (file-name-as-directory elpamr-default-output-directory))
                (t (read-directory-name "Output directory:"))))

    ;; Delete output directory if we need a clean output directory
    (when (and recreate-directory
               (file-directory-p output-directory))
      (message "Re-create %s" output-directory)
      (delete-directory output-directory t))

    ;; Create output directory if it does not exist.
    (unless (file-directory-p output-directory)
      (make-directory output-directory t))

    (when (and (> (length final-pkg-list) 0)
               output-directory
               (file-directory-p output-directory))

      (dolist (dir dirs)
        (unless (or (member dir '("archives" "." ".."))
                    (not (elpamr--extract-info-from-dir dir)))
          ;; create tar using GNU tar
          ;; BSD tar need set environment variable COPYFILE_DISABLE
          (setq tar-cmd (concat (if (elpamr--is-mac) "COPYFILE_DISABLE=\"\" " "")
                                (elpamr--executable-find "tar")
                                " cf "
                                (elpamr--fullpath output-directory dir)
                                ".tar --exclude=\"*.elc\" --exclude=\"*~\" "
                                ;; tar 1.14 NEWS,
                                ;; @see https://git.savannah.gnu.org/cgit/tar.git/plain/NEWS?id=release_1_14
                                ;; * New option --format allows to select the output archive format
                                ;; * The default output format can be selected at configuration time
                                ;;   by presetting the environment variable DEFAULT_ARCHIVE_FORMAT.
                                ;;   Allowed values are GNU, V7, OLDGNU and POSIX.
                                (if (elpamr--is-mac) "" " --format=gnu ")
                                " -C "
                                package-user-dir
                                " "
                                dir))

          ;; for windows
          (if elpamr-debug (message "tar-cmd=%s" tar-cmd))
          (shell-command tar-cmd)
          (setq cnt (1+ cnt))
          (message "Creating *.tar ... %d%%" (/ (* cnt 100) (length dirs)))))

      ;; output archive-contents
      (with-temp-buffer
        (let* ((print-level nil)
               (print-length nil))
          (insert "(1\n")
          (dolist (final-pkg final-pkg-list)
            ;; each package occupies one line
            (insert (elpamr--one-item-for-archive-contents final-pkg)))
          (insert ")"))
        (write-file (elpamr--fullpath output-directory
                                      "archive-contents" t)))
      (run-hook-with-args 'elpamr-finished-hook output-directory)
      (message "DONE! Output into %s" output-directory))))

(provide 'elpa-mirror)
;;; elpa-mirror.el ends here
