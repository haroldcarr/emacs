;;;;
;;;; Created       : 1999 Jun 11 (Fri) 10:18:26 by Harold Carr.
;;;; Last Modified : 1999 Aug 09 (Mon) 15:56:50 by Harold Carr.
;;;;

(defun hcShExecCmd (name &rest args)
  (shell-command-to-string
   (concat (symbol-name name) " "
	   (apply #'concat
		  (mapcar #'(lambda (arg) (format "%s " arg))
			  args)))))

(defmacro hcShDefCmd (name args)
  `(defun ,name ,args
     (apply #'hcShExecCmd (list ',name ,@args))))

(defmacro hcShDefCmdMemo (name)
  (let ((varName (intern (format "*%s*" name))))
    `(progn
       (defvar ,varName nil)
       (defun ,name ()
	 (cond (,varName)
	       (t (setq ,varName (hcShExecCmd ',name))))))))

(hcShDefCmdMemo hcPathSep)
(hcShDefCmd hcLibClasspath ())

(provide 'hcSh)

;;; End of file.