(setq elpamr-default-output-directory "~/myelpa")

(defun elpamr--create-one-item-for-archive-contents (pkg)
  (let ((name (car pkg))
        item
        package-content
        found
        (i 0))

    (while (and (not found)
                (< i (length package-archive-contents)))
      (setq package-content (nth i package-archive-contents))
      ;; well, all we need do it to write the actual version into package-content
      (when (string= name (car package-content))
        ;; we try to provide more information from archive-contents if possible
        (aset (cdr package-content) 0 (elt (cdr pkg) 0))
        (setq item package-content)
        (setq found t)
        )
      (setq i (1+ i)))

    (unless found
      (message "not found")
      ;; make do with installed package, looks it's deleted in archive-contents
      (setq item pkg))

    item
    ))

(defun elpamr--version-num-from-dirname (dirname)
  "return '(package-name integer-version-number) or nil"
  (interactive)
  (let (rlt)
    (setq rlt '(1000 "test1"))
    ))

(defun elpamr-create-mirror ()
  "export and packages pkg into a new directory.create all the necessary web files for a mirror site"
  (interactive)
  (let (item rlt pkg-dirname)
    (dolist (pkg package-alist)
      (setq item (elpamr--create-one-item-for-archive-contents pkg))
      (push item rlt)
      ;; (message "pkg=%s" pkg)
      )
    ;; (message "rlt=%s" rlt)
    ;; package the tar
    ;; instead we scan all the sub-directories in ~/.emacs.d/elpa
    (dolist (dir (directory-files package-user-dir))
      (unless  (member dir '("archives" "." ".."))
        (message "dir=%s" dir)
        )
      ;; extract dir with name and version, version should be converted into a number
      ;; compare with the version in the list (that version should also be a number
      ;; if both package name and version match, than package it, else just ignore
      ;; because we don't want to deal with orphan packages
      )
    ;; (dolist (pkg rlt)
    ;;   (setq pkg-dirname (elpamr--pkg-dirname pkg))
	;;   )
    ))

(provide 'elpa-mirror)