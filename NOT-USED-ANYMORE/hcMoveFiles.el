;;;
;;; Move the files
;;;

(defun hc-move-files (file-list from-dir to-dir)
  (save-selected-window
    (mapc '(lambda (x)
	     (if (equal (car x) 'file)
		 (let ((full-read-path (concat from-dir (car (cdr x))))
		       (full-write-path (concat to-dir (car (cdr x)))))
		   (find-file full-read-path)
		   (write-file full-write-path t)
		   (kill-buffer (current-buffer))
		   (princ (format "(changed %S)\n" full-write-path)))))
	  file-list)))
