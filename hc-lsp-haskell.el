;;; hc-lsp-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2022 Dec 02 (Fri) 08:18:36 by Harold Carr.
;;;;

;;; Code:

(use-package hc-lsp)

(add-hook 'haskell-mode-hook          #'lsp-mode)
(add-hook 'haskell-literate-mode-hook #'lsp-mode)

(use-package lsp-haskell
  :custom
  (lsp-haskell-brittany-on           nil)
  (lsp-haskell-floskell-on           nil)
  (lsp-haskell-fourmolu-on           nil)
  (lsp-haskell-ormolu-on             nil)
)

(provide 'hc-lsp-haskell)

;;; hc-lsp-haskell.el ends here

