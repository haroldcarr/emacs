;;; hc-vue-mode --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Aug 15 (Tue) 08:44:40 by Harold Carr.
;;;;

;;; Code:

;; https://www.reddit.com/r/emacs/comments/orxwhl/comment/h6u30qa/?utm_source=share&utm_medium=web2x&context=3

(eval-when-compile                (require 'use-package))
(cl-eval-when (compile load eval) (require 'web-mode))

(define-derived-mode hc-vue-mode web-mode "pbVue"
  "Major mode derived from web-mode, for editing .vue files with LSP support.")

(use-package hc-eglot
  :after eglot
  :config
  ;; (add-hook
  ;;  'eglot-managed-mode-hook
  ;;  (lambda ()
  ;;    (when (eglot-managed-p)
  ;;      (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  ;;      (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
  ;;      (define-key eglot-mode-map (kbd "C-c h") 'eldoc))))
  (add-to-list 'auto-mode-alist       '("\\.vue\\'" . hc-vue-mode))
  (add-hook    'hc-vue-mode-hook      'eglot-ensure)
  (add-to-list 'eglot-server-programs '(hc-vue-mode "vls"))
)

(provide 'hc-vue-mode)

;;; hc-vue-mode.el ends here

