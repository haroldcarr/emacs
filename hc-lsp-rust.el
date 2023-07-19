;;; hc-lsp-rust.el --- rust          -*- lexical-binding: t; -*-

;;; Commentary:
;; based on https://robert.kra.hn/posts/rust-emacs-setup/

;;; Code:

(eval-when-compile (require 'use-package))

(use-package hc-lsp)

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

(provide 'hc-lsp-rust)

;;; hc-lsp-rust.el ends here
