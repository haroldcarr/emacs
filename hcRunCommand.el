;;;;
;;;; Created       : a long time ago ...        by Harold Carr.
;;;; Last Modified : 2011 Apr 03 (Sun) 21:02:33 by carr.
;;;;

(require 'comint)
(autoload 'run-scheme  "cmuscheme" "Run an inferior Scheme process." t)
;;(setq hcScmPopToBuffer nil) ; add this to cmuscheme.el run-scheme

(defmacro hcRunCommand (command &rest path-and-command-line-args)
  (let* ((command-name (symbol-name command))
	 (buf-name     (concat "*" command-name "*")))
    `(progn
       (defun ,command ()
	 ,(concat "A shell for executing " command-name)
	 (interactive)
	 (let* ((buffer-name ,buf-name)
		(existing-buffer (get-buffer buffer-name))
		(sb nil))
	   (cond (existing-buffer
		  (setq sb existing-buffer))
		 (t
		  (run-scheme ,path-and-command-line-args)
		  (setq sb (get-buffer "*scheme*"))
		  (save-excursion
		    (set-buffer sb)
		    (rename-buffer buffer-name))))
	   (let ((sw (if (one-window-p t 'selected-frame)
			 (split-window-vertically)
		       (next-window))))
	     (setq scheme-buffer buffer-name)
	     (set-window-buffer sw sb))))
       ;; So you can have multiple scheme shells and pick one
       ;; (via its buffer name) to receive input.
       (defun ,(intern buf-name) ()
	 ,(concat "Sets variable 'scheme-buffer' to " buf-name)
	 (interactive)
	 (setq scheme-buffer ,buf-name))
       (defun ,(intern (concat "k" command-name)) ()
	 ,(concat "Kill the " buf-name " buffer")
	 (interactive)
	 (kill-buffer ,buf-name)))))

(provide 'hcRunCommand)

;;; End of file.
