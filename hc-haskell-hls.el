;; https://github.com/haskell/haskell-language-server

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp)

(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package lsp-haskell
  :custom
  (lsp-ui-doc-position 'bottom)
  :custom-face
  (lsp-ui-doc-background ((t (:background "white"))))
)

(provide 'hc-haskell-hls)

