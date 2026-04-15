;;; hc-sdedit.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;; http://sdedit.sourceforge.net/
;; Send diagram text to SDEDIT (UML sequence diagrams)

;;; Code:

;; When the current buffer contains SDEDIT diagram text, just do
;; M-x sdedit

;; Be sure the sdedit program is up and running as a service.

(defun hcSdedit () "."
  (interactive)
  (let ((p (open-network-stream "*HC-SDEDIT*" "*HC-SDEDIT-CONNECTION*" "localhost" "60001")))
    (process-send-string p (concat (buffer-name) "
" (buffer-string)))
    (delete-process p)))

(provide 'hc-sdedit)

;;; hc-sdedit.el ends here
