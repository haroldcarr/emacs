;; https://github.com/haskell/haskell-language-server

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp)

(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(defvar hc-lsp-mode-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix hc-lsp-mode-keymap-prefix))
                        (lsp-enable-which-key-integration))))
  :config
    (define-key lsp-mode-map (kbd hc-lsp-mode-keymap-prefix) lsp-command-map)
    ;; https://emacs.stackexchange.com/a/54976/5176
    (setq lsp-file-watch-threshold 512
          lsp-enable-file-watchers nil)
)

(use-package lsp-haskell
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable nil)
  :custom-face ;; gray35/tango-dark; white/zenburn
  (lsp-ui-doc-background ((t (:background "white" ;; "gray35"
                                          ))))
)

(require 'lsp-treemacs)

(provide 'hc-haskell-hls)

