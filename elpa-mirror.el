(defun elpa-mirror-create-mirror ()
  "export and packages installed into a new directory.create all the necessary web files for a mirror site"
  (interactive)
  (let (name)
    (dolist (elt package-alist)
      (setq name (car elt))
        (message "name=%s" name)
        (message "elt=%s" elt)

        )
    ))

(provide 'elpa-mirror)