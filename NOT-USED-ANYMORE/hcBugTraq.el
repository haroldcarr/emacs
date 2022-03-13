;;;;
;;;; Created       : 2001 Jan 30 (Tue) 15:48:55 by Harold Carr.
;;;; Last Modified : 2001 Jan 30 (Tue) 16:05:35 by Harold Carr.
;;;;

(comment
(defun pb-find-default()
  (let ((pair (thing-symbol (point))))
    (and pair
         (buffer-substring (car pair) (cdr pair)))))
)

(defun pb-find-default () nil)

(defun pb-pb (my-prompt)
  (let* ((default (pb-find-default))
	 (bugnum
          (completing-read
           (if default
               (format "%s(default %s) " my-prompt default)
             my-prompt)
           nil)))
    (if (string-equal bugnum "")
        ;; #### - This is a really LAME way of doing it!  --Stig
        default                 ;indicate exact symbol match
      bugnum)))

(defun pb (bugnum &optional other-window)
  "*Show bug report for bugnum."
  (interactive (if current-prefix-arg
                   '(nil nil)
                 (list (pb-pb "Find bug: ") nil)))
  ;; REVISIT: Pipe the output of the cmd directly into a buffer.
  (setq jj (shell-command-to-string (concat "bugprint -U hc26044 " bugnum)))
  (pop-to-buffer bugnum)
  (insert-string jj)
  (beginning-of-buffer))

;;; End of file.


