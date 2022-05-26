;;; elpa-mirror-test-bsd-tar.el ---  unit tests for elpa-mirror -*- coding: utf-8 -*-

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
(require 'elpa-mirror-test-common)

(ert-deftest elpa-mirror-test-bsd-tar ()
  (elpa-mirror-test-tar-program "tar"))

(ert-run-tests-batch-and-exit)
;;; elpa-mirror-test-bsd-tar.el ends here