;;; hc-lsp-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2024 Mar 29 (Fri) 11:17:00 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(use-package hc-lsp)

(add-hook 'haskell-mode-hook          #'lsp-mode)
(add-hook 'haskell-literate-mode-hook #'lsp-mode)

(use-package lsp-haskell
  :custom
  (lsp-haskell-brittany-on           nil)
  (lsp-haskell-floskell-on           nil)
  (lsp-haskell-fourmolu-on           nil)
  (lsp-haskell-ormolu-on             nil)
  (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on nil)
  (lsp-haskell-formatting-provider   "stylish-haskell")
)

(add-hook 'lsp-after-initialize-hook
          '(lambda ()
             (lsp--set-configuration
              '(:haskell
                (:formattingProvider "stylish-haskell")
                (:plugin
                 (:tactics (:config (:timeout_duration 5)))
                 (:ghcide-completions (:config (:autoExtendOn false)))
                )
               )
           )))

(provide 'hc-lsp-haskell)

;;; hc-lsp-haskell.el ends here

