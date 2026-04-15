;; https://apps.bram85.nl/git/bram/kagi.el

(use-package kagi
  :custom
  (kagi-api-token (lambda ()
                    (let* ((auth (car (auth-source-search :host "api.kagi.com" :require '(:secret))))
                           (token (plist-get auth :secret)))
                      (funcall token))))
  ;; Universal Summarizer settings
  (kagi-summarizer-engine "cecil")
  (kagi-summarizer-default-language "EN")
  (kagi-summarizer-cache t)

  :custom-face
  ;; kagi-code defaults to fixed-pitch, overridden here:
  (kagi-code ((t (:inherit org-verbatim))))
  ;; override kagi-bold:
  (kagi-bold ((t (:inherit modus-themes-bold)))))

(defun hc-kagi-auth ()
  "Attempt to get the API token for Kagi."
  (unless kagi-api-token
    (when-let ((auth (car (auth-source-search :host "api.kagi.com" :require '(:secret))))
               (token (plist-get auth :secret)))
      (setq kagi-api-token (funcall token)))))

(provide 'hc-chat-gpt)
