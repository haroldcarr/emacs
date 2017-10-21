;;; hc-org-mobile --- Org Mobile

;;; Commentary:
;;;          http://orgmode.org/org.html#MobileOrg
;;; iPhone:  http://mobileorg.ncogni.to/
;;; android: https://github.com/matburt/mobileorg-android/wiki

;;; Code:

(defun hcOrgMobile ()
  (setq org-directory                  (concat (hcFtptmp)    "/org-directory"))           ; location of org files (TODO: sym links?)
  (setq org-mobile-files              '(org-agenda-files))                                ; use ones I list in agenda
  (setq org-mobile-directory           (concat (hcFtptmp)    "/Dropbox/apps/MobileOrg"))  ; where interaction with mobile happens
  (setq org-mobile-use-encryption      t)
  (setq org-mobile-encryption-tempfile (concat org-directory "/orgtmpcrypt"))             ; only you should have access
  (setq org-mobile-encryption-password "")                                                ; do NOT set : will prompt
  (setq org-mobile-inbox-for-pull      (concat org-directory "/from-mobile.org"))         ; caputre notes/flags appended here
)

(use-package async
  :defer t
  :config
  (use-package org
    :config
    (hcOrgMobile)))

;; https://github.com/jwiegley/dot-emacs/blob/master/dot-org.el
(defun hc-org-mobile-pre-pull-function ()
  (async-start
   (lambda ()
     (shell-command "open /Applications/Dropbox.app")
     (sleep-for 30)
     (shell-command "osascript -e 'tell application \"Dropbox\" to quit'"))
   ignore))

(defun hc-org-mobile-post-push-function ()
  (async-start
   (lambda ()
     (shell-command "open /Applications/Dropbox.app")
     (sleep-for 30)
     (shell-command "osascript -e 'tell application \"Dropbox\" to quit'"))
   'ignore))

(add-hook 'org-mobile-pre-pull-hook  'hc-org-mobile-pre-pull-function)
(add-hook 'org-mobile-post-push-hook 'hc-org-mobile-post-push-function)

(provide 'hc-Org-Mobile)

;;; hc-org-mobile.el ends here
