;; https://github.com/haskell/haskell-language-server
;; https://ianyepan.github.io/posts/emacs-ide/

(add-hook 'haskell-mode-hook          #'lsp-mode)
(add-hook 'haskell-literate-mode-hook #'lsp-mode)

(defvar hc-lsp-mode-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix hc-lsp-mode-keymap-prefix))
                        (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd hc-lsp-mode-keymap-prefix) lsp-command-map)
  ;; https://emacs.stackexchange.com/a/54976/5176
  (setq lsp-enable-file-watchers nil
        lsp-enable-snippet       nil
        lsp-file-watch-threshold 512
        )
  :custom
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;;(lsp-completion-provider            :capf)
;;(lsp-completion-show-detail         t)
;;(lsp-completion-show-kind           t)
;;(lsp-diagnostics-provider           :auto)
  (lsp-eldoc-enable-hover             t)
  (lsp-enable-symbol-highlighting     t)
  (lsp-headerline-breadcrumb-enable   t)
  (lsp-lens-enable                    t)
  (lsp-modeline-code-actions-enable   t)
  (lsp-modeline-diagnostics-enable    t)
  ;; manual request via `lsp-signature-activate`
;;(lsp-signature-auto-activate        '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
)

(add-hook 'haskell-mode-hook          #'lsp-mode)
(add-hook 'haskell-literate-mode-hook #'lsp-mode)

(use-package lsp-ui
;;:custom-face ;; gray35/tango-dark; white/zenburn; autumn-light/mv/gray50
;;(lsp-ui-doc-background              ((t (:background "gray50"))))
  :custom
  (lsp-ui-doc-enable                 t)
  (lsp-ui-doc-position               'top)
  (lsp-ui-doc-show-with-cursor       t)
  (lsp-ui-doc-show-with-mouse        t)
  (lsp-ui-sideline-enable            t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover        nil)
)

(use-package lsp-haskell
  :custom
  (lsp-haskell-brittany-on           nil)
  (lsp-haskell-floskell-on           nil)
  (lsp-haskell-fourmolu-on           nil)
  (lsp-haskell-ormolu-on             nil)
)

;;(require 'lsp-treemacs)

(provide 'hc-haskell-hls)

