;;; hc-juno-log-follower.el --- help with juno logs          -*- lexical-binding: t; -*-

;;; Commentary:

;; Put your point on a message id in a log file.
;; M-x hcjlf/findMatch
;; That will open the corresponding file and put you at the line with the FIRST match.

;; Suggest binding hcjlf/findMatch to a key of your choosing.

;; NOTE: this code has NO error handling (e.g., invoking hcjlf/findMatch outside of a message id).

;;; Code:

(eval-when (compile)
  (require 'cl))

(defun hcjlf/findMatch ()
  (interactive)
  (let* ((msgId (hcjlf/getMsgId))
         (file (car (hcjlf/findFileForPort (concat default-directory "../../")
                                           (hcjlf/msgIdToPort msgId)))))
    (find-file file)
    (search-forward msgId)))

(defun hcjlf/getMsgId ()
  (let ((userLocation (point)))
    (search-backward " ")
    (let ((beg (point)))
      (search-forward ";")
      (let ((end (point)))
        (kill-ring-save beg (- end 1))
        (goto-char userLocation)
        (current-kill 0)))))

(defun hcjlf/msgIdToPort (mid)
  (destructuring-bind (_ _ portAndMsgNum) (split-string mid ":" mid)
    (destructuring-bind (port _) (split-string portAndMsgNum "-")
      port)))

(defun hcjlf/findFileForPort (logDir port)
  (directory-files-recursively logDir (concat ".*" port ".*")))

(provide 'hc-juno-log-follower)

;;; hc-juno-log-follower.el ends here
