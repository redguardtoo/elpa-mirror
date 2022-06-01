;;; elpa-mirror-test-deps.el --- prepare test environment  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Chen Bin
;;

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.


;;; Code:

(require 'package)
(require 'elpa-mirror-test-common)

(package-initialize)
(package-refresh-contents)
(dolist (pkg '(cobol-mode lex))
  (message "installing package: %s" pkg)
  (unless (package-installed-p pkg) (package-install pkg)))

(defun elpa-mirror-test-create-myelpa (tar-program)
  (let* ((elpamr-default-output-directory (elpa-mirror-get-elpa-output-directory tar-program))
         (elpamr-tar-executable tar-program))
    ;; now produce packages in output directory
    (elpamr-create-mirror-for-installed)
    (sit-for 3)))

(elpa-mirror-test-create-myelpa "tar")
(elpa-mirror-test-create-myelpa "bsdtar")

(provide 'elpa-mirror-test-deps)
;;; elpa-mirror-test-deps.el ends here