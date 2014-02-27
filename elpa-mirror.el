(defvar elpamr-default-output-directory
  nil
  "The output directory. If nil, user will be required provide one when running `elpamr-create-mirror-for-installed`")

(defvar elpamr-repository-name
  "myelpa"
  "repository name. It will be displayed in index.html.")

(defvar elpamr-repository-path
  "http://myelpa.mydomain.com"
  "Repository path. It could be a local directory. The file `archive-contents` will be fetched from it.")

(defvar elpamr-email
  nil
  "Email. If nil, the user-mail-address will be used.")

(defun elpamr--create-one-item-for-archive-contents (pkg)
  "We can use package-alist directly. This one just append my meta info into package-alist"
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
      ;; make do with installed package, looks it's deleted in archive-contents
      (setq item pkg))

    (let ((a (cdr item)) na)
      (when (= 5 (length a))
        ;; only need first four
        (setq na (vector (elt a 0)
                             (elt a 1)
                             (elt a 2)
                             (elt a 3)
                             ))
        (setq item (cons (car item) na))))
    item
    ))


(defun elpamr--package-info (dirname)
  "return '(package-name integer-version-number) or nil"
  (interactive)
  (let (rlt name version)
    (when (string-match "\\(.*\\)-\\([0-9.]+\\)$" dirname)
      (setq name (match-string 1 dirname))
      (setq version (split-string (match-string 2 dirname) "\\."))
      (setq rlt (list name version)))
    rlt
    ))

(defun elpamr--output-fullpath (file)
  "return full path of output file give the FILE"
  (file-truename (concat
                  (file-name-as-directory elpamr-default-output-directory)
                  file)))

(defun elpamr--get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun elpamr--clean-package-description (descr)
  (replace-regexp-in-string "-\*-.*-\*-" "" descr t))

(defun elpamr--create-complete-package-name (item)
  (concat (symbol-name (car item))
          "-"
          (mapconcat (lambda (arg) (format "%d" arg)) (elt (cdr item) 0) ".")))

(defun elpamr--format-package-list-into-json (list)
  (let (pkg-name)
    (mapconcat
     (lambda (item)
       (setq pkg-name (elpamr--create-complete-package-name item))
       (format "'%s'" pkg-name)
       ) list ",\n")
    ))

(defun elpamr--format-package-list-into-html (list)
  (let (tar-name (cnt 0))
    (mapconcat
     (lambda (item)
       (setq cnt (1+ cnt))
       (setq tar-name (concat (elpamr--create-complete-package-name item) ".tar"))
       (format "<div id='n%d' class='name'><a href='%s'>%s</a></div><div id='d%d' class='descr'>%s</div>\n"
               cnt
               tar-name
               tar-name
               cnt
               (elpamr--clean-package-description (elt (cdr item) 2)))
       ) list "\n")
    ))

(defun elpamr--format-email ()
  (let ((email (if elpamr-email elpamr-email (if user-mail-address user-mail-address ""))))
    (format "<a href='mailto:%s'>%s</a>" email email)
    ))

(defun elpamr--output-html (rlt)
  (let ((js-file (elpamr--output-fullpath "elpa-mirror.js"))
        (js-tmpl (concat
                  (file-name-directory (if load-file-name load-file-nam (symbol-file 'elpamr--output-html)))
                  "elpa-mirror.js"))
        (html-file (elpamr--output-fullpath "index.html"))
        ;; @see http://stackoverflow.com/questions/145291/smart-home-in-emacs/145359#145359
        (html-tmpl (concat
                    (file-name-directory (if load-file-name load-file-nam (symbol-file 'elpamr--output-html)))
                    "index.html")))

    ;; index.html
    (with-temp-buffer
      (let ((print-level nil)  (print-length nil) str)
        (setq str (replace-regexp-in-string
                 "elpamr-package-list-html"
                 (elpamr--format-package-list-into-html rlt)
                 (elpamr--get-string-from-file html-tmpl)
                 t))
        (setq str (replace-regexp-in-string
                   "elpamr-package-list-json"
                   (elpamr--format-package-list-into-json rlt)
                   str
                   t))
        (setq str (replace-regexp-in-string
                   "elpamr-email"
                   (elpamr--format-email)
                   str
                   t))
        (setq str (replace-regexp-in-string
                   "elpamr-repository-name"
                   elpamr-repository-name
                   str
                   t))
        (setq str (replace-regexp-in-string
                   "elpamr-repository-path"
                   elpamr-repository-path
                   str
                   t))
        (insert str))
      (write-file html-file))

    ;; js file
    (with-temp-buffer
      (let ((print-level nil)  (print-length nil))
        (insert (elpamr--get-string-from-file js-tmpl)))
      (write-file js-file))
    ))

;;;###autoload
(defun elpamr-create-mirror-for-installed ()
  "Export INSTALLED packages into a new directory. Create html files for the mirror site.
If elpamr-default-output-directory is not nil, it's assumed that is output directory. Or else, user will be asked to provide the output directory."
  (interactive)
  (let (item rlt pkg-dirname pkg-info tar-cmd len dirs)
    (dolist (pkg package-alist)
      (setq item (elpamr--create-one-item-for-archive-contents pkg))
      (push item rlt)
      )

    (unless (and elpamr-default-output-directory (file-directory-p elpamr-default-output-directory))
      (setq elpamr-default-output-directory (read-directory-name "Output directory:"))
      )

    (when (and elpamr-default-output-directory (file-directory-p elpamr-default-output-directory))
      (setq dirs (directory-files package-user-dir))
      (setq len (length dirs))
      (dolist (dir dirs)
        (unless (or (member dir '("archives" "." ".."))
                    (not (setq pkg-info (elpamr--package-info dir))))
          ;; create tar
          (setq tar-cmd (concat "cd " package-user-dir "; tar cf " (elpamr--output-fullpath dir) ".tar --exclude=*.elc --exclude=*~ " dir))
          (shell-command tar-cmd)))

      ;; output archive-contents
      (with-temp-buffer
        (let ((print-level nil)  (print-length nil))
          (insert (format "%S" (cons 1 rlt))))
        (write-file (elpamr--output-fullpath "archive-contents")))
      (elpamr--output-html rlt))
    ))

(provide 'elpa-mirror)