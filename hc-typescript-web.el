;;; hc-typescript-web.el --- xxx                  -*- lexical-binding: t; -*-

;;;;; ***** I HAVE NOT TRIED OR USED THIS

;;; Commentary:

;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config

;;; Code:


;; (use-package treesit
;;       :mode (("\\.tsx\\'" . tsx-ts-mode)
;;              ("\\.js\\'"  . typescript-ts-mode)
;;              ("\\.mjs\\'" . typescript-ts-mode)
;;              ("\\.mts\\'" . typescript-ts-mode)
;;              ("\\.cjs\\'" . typescript-ts-mode)
;;              ("\\.ts\\'"  . typescript-ts-mode)
;;              ("\\.jsx\\'" . tsx-ts-mode)
;;              ("\\.json\\'" .  json-ts-mode)
;;              ("\\.Dockerfile\\'" . dockerfile-ts-mode)
;;              ("\\.prisma\\'" . prisma-ts-mode)
;;              ;; More modes defined here...
;;              )
;;       :preface
;;       (defun os/setup-install-grammars ()
;;         "Install Tree-sitter grammars if they are absent."
;;         (interactive)
;;         (dolist (grammar
;;                  '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                    (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;                    (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
;;                    (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                    (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;                    (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
;;                    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;                    (make "https://github.com/alemuller/tree-sitter-make")
;;                    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;                    (cmake "https://github.com/uyha/tree-sitter-cmake")
;;                    (c "https://github.com/tree-sitter/tree-sitter-c")
;;                    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;                    (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;                    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;                    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                    (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
;;                    (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
;;           (add-to-list 'treesit-language-source-alist grammar)
;;           ;; Only install `grammar' if we don't already have it
;;           ;; installed. However, if you want to *update* a grammar then
;;           ;; this obviously prevents that from happening.
;;           (unless (treesit-language-available-p (car grammar))
;;             (treesit-install-language-grammar (car grammar)))))

;;       ;; Optional, but recommended. Tree-sitter enabled major modes are
;;       ;; distinct from their ordinary counterparts.
;;       ;;
;;       ;; You can remap major modes with `major-mode-remap-alist'. Note
;;       ;; that this does *not* extend to hooks! Make sure you migrate them
;;       ;; also
;;       (dolist (mapping
;;                '((python-mode . python-ts-mode)
;;                  (css-mode . css-ts-mode)
;;                  (typescript-mode . typescript-ts-mode)
;;                  (js-mode . typescript-ts-mode)
;;                  (js2-mode . typescript-ts-mode)
;;                  (c-mode . c-ts-mode)
;;                  (c++-mode . c++-ts-mode)
;;                  (c-or-c++-mode . c-or-c++-ts-mode)
;;                  (bash-mode . bash-ts-mode)
;;                  (css-mode . css-ts-mode)
;;                  (json-mode . json-ts-mode)
;;                  (js-json-mode . json-ts-mode)
;;                  (sh-mode . bash-ts-mode)
;;                  (sh-base-mode . bash-ts-mode)))
;;         (add-to-list 'major-mode-remap-alist mapping))
;;       :config
;;       (os/setup-install-grammars))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq major-mode-remap-alist
      '(
        (bash-mode       . bash-ts-mode)
        (css-mode        . css-ts-mode)
        (js2-mode        . js-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        ))

;; ;;;; Code Completion
;; (use-package corfu
;;   :ensure t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
;;   (corfu-auto-delay 0)            ; No delay for completion
;;   (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
;;   (corfu-preview-current 'insert) ; insert previewed candidate
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;;               ("M-SPC"      . corfu-insert-separator)
;;               ("TAB"        . corfu-next)
;;               ([tab]        . corfu-next)
;;               ("S-TAB"      . corfu-previous)
;;               ([backtab]    . corfu-previous)
;;               ("S-<return>" . corfu-insert)
;;               ("RET"        . corfu-insert))

;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;;               (corfu-mode))
;;             nil
;;             t))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :bind (:map flycheck-mode-map
;;               ("M-n" . flycheck-next-error) ; optional but recommended error navigation
;;               ("M-p" . flycheck-previous-error)))

(use-package lsp-mode
      :diminish "LSP"
      :ensure t
      :hook ((lsp-mode . lsp-diagnostics-mode)
             (lsp-mode . lsp-enable-which-key-integration)
             ((tsx-ts-mode
               typescript-ts-mode
               js-ts-mode) . lsp-deferred))
      :custom
      (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
      (lsp-completion-provider :none)       ; Using Corfu as the provider
      (lsp-diagnostics-provider :flycheck)
      (lsp-session-file (locate-user-emacs-file ".lsp-session"))
      (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
      (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
      (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
      ;; core
      (lsp-enable-xref t)                   ; Use xref to find references
      (lsp-auto-configure t)                ; Used to decide between current active servers
      (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
      (lsp-enable-dap-auto-configure t)     ; Debug support
      (lsp-enable-file-watchers nil)
      (lsp-enable-folding nil)              ; I disable folding since I use origami
      (lsp-enable-imenu t)
      (lsp-enable-indentation nil)          ; I use prettier
      (lsp-enable-links nil)                ; No need since we have `browse-url'
      (lsp-enable-on-type-formatting nil)   ; Prettier handles this
      (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
      (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
      (lsp-enable-text-document-color nil)   ; This is Treesitter's job

      (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
      (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
      ;; completion
      (lsp-completion-enable t)
      (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
      (lsp-enable-snippet t)                         ; Important to provide full JSX completion
      (lsp-completion-show-kind t)                   ; Optional
      ;; headerline
      (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
      (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
      (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
      (lsp-headerline-breadcrumb-icons-enable nil)
      ;; modeline
      (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
      (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
      (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
      (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
      (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
      (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
      ;; lens
      (lsp-lens-enable nil)                 ; Optional, I don't need it
      ;; semantic
      (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

      :init
      (setq lsp-use-plists t))

    (use-package lsp-completion
      :no-require
      :hook ((lsp-mode . lsp-completion-mode)))

    (use-package lsp-ui
      :ensure t
      :commands
      (lsp-ui-doc-show
       lsp-ui-doc-glance)
      :bind (:map lsp-mode-map
                  ("C-c C-d" . 'lsp-ui-doc-glance))
      :after (lsp-mode evil)
      :config (setq lsp-ui-doc-enable t
                    evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                    lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                    lsp-ui-doc-include-signature t       ; Show signature
                    lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;;; hc-typescript-web.el ends here
