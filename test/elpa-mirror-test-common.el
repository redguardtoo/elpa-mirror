;;; elpa-mirror-test-common.el ---  common files for elpa-mirror test -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; Code:

(require 'ert)
(require 'elpa-mirror)
(require 'find-lisp)

(defun elpa-mirror-test-files-in-output-directory ()
  "List files in output directory."
  (find-lisp-find-files-internal
   elpamr-default-output-directory
   (lambda (file dir) (not (file-directory-p (expand-file-name file dir))))
   (lambda (file dir) nil)))

(defun elpa-mirror-test-tar-summary (tar-file)
  "Summary of TAR-FILE."
  (shell-command-to-string (format "%s -tf %s" tar-program tar-file)))

(defun elpa-mirror-get-elpa-output-directory (tar-program)
  "Return output directory per TAR-PROGRAM."
  (concat my-test-dir "/" tar-program))

(defun elpa-mirror-test-tar-program (tar-program)
  "Test TAR-PROGRAM."
  (let* ((elpamr-default-output-directory (elpa-mirror-get-elpa-output-directory tar-program))
         output-files)

    (setq output-files (elpa-mirror-test-files-in-output-directory))
    (should (>= (length output-files) 3))

    (dolist (file output-files)
      (cond
       ((string-match "archive-contents$" file)
        (let ((file-content (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string))))
          (should (string-match "(csv-mode " file-content))
          (should (string-match "(lex " file-content))))

       ((string-match "/lex\.tar$" file)
        (should (string-match "/lex" (elpa-mirror-test-tar-summary file))))

       ((string-match "/csv-mode.*\.tar$" file)
        (should (string-match "/csv-mode.el$" (elpa-mirror-test-tar-summary file))))))))

(provide 'elpa-mirror-test-common)
;;; elpa-mirror-test-common.el ends here