;;; hc-rust.el --- rust          -*- lexical-binding: t; -*-

;;; Commentary:
;; based on https://robert.kra.hn/posts/rust-emacs-setup/

;;; Code:

;; rustic = basic rust-mode + additions
(use-package rustic
  :ensure nil
  :bind (:map rustic-mode-map
              ("M-j"       . lsp-ui-imenu)
              ("M-?"       . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c h" . lsp-ui-doc-glance))
  )

;; for rust-analyzer integration
(use-package lsp-mode
  :ensure nil
  :commands lsp
  :custom
  ;; (lsp-eldoc-render-all                                                 t)
  ;; (lsp-idle-delay                                                       0.6)
  ;; What to use when checking on-save. "check" is default.
  (lsp-rust-analyzer-cargo-watch-command                                "clippy")
  ;; This controls the overlays that display type and other hints inline.
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable              "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints                             t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints                  t)
  (lsp-rust-analyzer-display-parameter-hints                            nil)
  (lsp-rust-analyzer-display-reborrow-hints                             nil)
  (lsp-rust-analyzer-server-display-inlay-hints                         t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; inline errors
(use-package flycheck :ensure nil)

;; Create / cleanup rust scratch projects quickly
(use-package rust-playground :ensure nil)

;; for Cargo.toml and other config files
(use-package toml-mode :ensure nil)


(provide 'hc-rust)
;;; hc-rust.el ends here
