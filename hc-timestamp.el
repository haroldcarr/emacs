;;; hc-haskell-init --- File timestamps (from Glickstein's _GNU Emacs Extensions_).

;;; Commentary:

;;;;
;;;; Created       : 1997 Jul 02 (Wed) 00:28:06 by Harold Carr.
;;;; Last Modified : 2013 Mar 21 (Thu) 13:36:36 by Harold Carr.
;;;;

;;; Code:

;; p 56

(defvar createstamp-prefix "Created       : "
  "*String identifying start of createstamp.")

(defvar modifystamp-prefix "Last Modified : "
  "*Unique string identifying start of modifystamp.")

(defvar logstamp-prefix    "Log : "
  "*Unique string identifying start of logstamp.")

(defvar modifystamp-format "%Y %b %d (%a) %X"
  "*Format for modifystamps (c.f. 'format-time-string').")

;; This is a procedure so it can pick up late binding of user-full-name.
;; Workaround for timing problem on pinyon-slc.
(defun modifystamp-after-time () (concat " by " (user-full-name)))
  "*String to be placed after date."

(defvar modifystamp-suffix "."
  "*String that terminates a modifystamp.")

;;

(defun insert-create-or-modifystamp (create-or-modify)
  (insert (concat create-or-modify
		  (format-time-string modifystamp-format (current-time))
		  (modifystamp-after-time)
		  modifystamp-suffix)))

(defun insert-modifystamp ()
  "Insert time and person modifying this file."
  (interactive)
  (insert-create-or-modifystamp modifystamp-prefix)
  (insert ?\n))

(defun insert-logstamp ()
  "Insert time and person making log entry."
  (interactive)
  (insert-create-or-modifystamp logstamp-prefix)
  (insert ?\n))

(defun insert-create-and-modifystamp ()
  "Insert time and person creating this file."
  (interactive)
  (insert-create-or-modifystamp createstamp-prefix)
  (insert ?\n)
  (insert-modifystamp))

;; p 67

(defvar last-change-time nil
  "Time of last buffer modification.")

(make-variable-buffer-local 'last-change-time)

(defun remember-change-time (&rest unused)
  "Store the current time in 'last-change-time'"
  (setq last-change-time (current-time)))

;; p 69

(defun update-modifystamps (time)
  "Find modifystamps and replace them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
       (widen)
       (goto-char (point-min))
       (let ((regexp (concat (regexp-quote modifystamp-prefix)
			     "\\(.*\\)"
			     (regexp-quote modifystamp-suffix)
			     "$")))
	 (while (re-search-forward regexp nil t)
	   ;; Note: different from p 64 of book.
	   ;; Subexpression not supported.
	   (replace-match (concat modifystamp-prefix
				  (format-time-string modifystamp-format
						      time)
				  (modifystamp-after-time)
				  modifystamp-suffix)))))))
  (setq last-change-time nil)
  nil)

;; Note: My hooks are on all buffers.

;;(make-local-hook 'after-change-functions)

(add-hook 'after-change-functions 'remember-change-time)

(add-hook 'write-file-hooks ;;'local-write-file-hooks
	  '(lambda ()
	     (if last-change-time
		 (update-modifystamps last-change-time))))

;; p 74.

(provide 'hc-timestamp)

;;; hc-timestamp.el ends here
