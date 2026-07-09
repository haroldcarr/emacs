;;; hc-pkms --- Personal Knowledge Management System(s)            -*- lexical-binding: t; -*-

;;; Commentary:

;; ...

;;;;
;;;; Created       : 2026 Jul 08 (Wed) 14:56:06 by Harold Carr.
;;;; Last Modified : 2026 Jul 08 (Wed) 20:47:59 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

;; https://www.chiply.dev/post-hyperbole-hywiki
;; https://rswgnu-hyperbole.mintlify.app/configuration/settings

(use-package hyperbole
  :custom
  (hywiki-directory (concat (hcLocation 'emacs) "/hywiki/"))
  ;; t        = Hyperbole M-RET handles more Org contexts
  ;; 'buttons = Hyperbole M-RET mainly handles buttons/links
  ;; nil      = leave Org M-RET alone
  ;;(hsys-org-enable-smart-keys 'buttons)
  (hsys-org-enable-smart-keys t)
  :config
  (hyperbole-mode 1) ;; C-h h h m a ;; HyWikiWords active everywhere
  (hywiki-mode :all) ;; C-h h h o a ;; M-RET does Hyperbole actions in Org mode
)

(provide 'hc-pkms)

;;; hc-pkms.el ends here
