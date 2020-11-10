;; https://github.com/haskell/haskell-language-server

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp)

(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(provide 'hc-haskell-hls)

