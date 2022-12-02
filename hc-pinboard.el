;;; hc-pinboard --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2022 Oct 28 (Fri) 12:19:26 by Harold Carr.
;;;;

;;; Code:

;;; Pinboard Packages


(defun hc-pick-pinboard-support ()
  "Prompt pick from a list."
  (interactive)
  (let ((choices '("pinboard-el" "pinboard-list")))
    (message "%s" (ido-completing-read "which pinboard?: " choices))))

(defun hc-pinboard ()
  "Pinboard."
  (interactive)
  (let ((pick (hc-pick-pinboard-support)))
    (cond ((equal pick "pinboard-el")
           (message (concat "using " pick))
           (setq hc-pinboard pick)
           (use-package hc-pinboard-el)
           (pinboard))
          ((equal pick "pinboard-list")
           (message (concat "using " pick))
           (use-package pinboard-api :ensure nil)
           (setq hc-pinboard pick)
           (use-package hc-pinboard-list
             :config
             ;; "Get the API token for Pinboard."
             (when-let ((auth (car (auth-source-search :host "api.pinboard.in"
                                                       :requires '(secret))))
                        (token (plist-get auth :secret)))
               (setq pinboard-api-token (funcall token))
               (message (concat "setq pinboard-api-token " pinboard-api-token)))
           )
           (pinboard-list-bookmarks))
          (t
           (message "NO PINBOARD MATCH %s" pick)))))

;;;;;;;;;;;;;

(provide 'hc-pinboard)

;;; hc-pinboard.el ends here

