;;; hc-eglot-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Jul 18 (Tue) 18:37:25 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(use-package hc-lsp)

;;(add-hook 'haskell-mode-hook          #'lsp-mode)
;;(add-hook 'haskell-literate-mode-hook #'lsp-mode)

;; https://haskell-language-server.readthedocs.io/en/latest/configuration.html#use-package-eglot

(use-package eglot
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

(provide 'hc-eglot-haskell)

;;; hc-eglot-haskell.el ends here

