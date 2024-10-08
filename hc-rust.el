;;; hc-rust.el --- rust          -*- lexical-binding: t; -*-

;;; Commentary:
;; based on https://robert.kra.hn/posts/rust-emacs-setup/

;;; Code:

(eval-when-compile (require 'use-package))

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

;; ;; https://www.reddit.com/r/emacs/comments/1b1d9l6/dapmode_lldb_debug_template_for_rust/
;; (add-hook 'rustic-mode-hook (lambda ()
;;    (dap-register-debug-template "Rust LLDB Debug Configuration"
;; 	  (list :type "cppdbg"
;; 	      :request "launch"
;; 	      :name "Rust::Run"
;; 	      :MIMode "lldb"
;; 	      :gdbpath "rust-lldb"
;; 	      :program (concat (projectile-project-root) "target/debug/" (projectile-project-name)) ;; Requires that the rust project is a project in projectile
;; 	      :environment []
;; 	      :cwd (projectile-project-root)))))

;; (use-package dap-cpptools)
;; ;;(use-package dap-gdb)

(use-package hc-flycheck)

;; create / cleanup rust scratch projects quickly
(use-package rust-playground :ensure nil)

;; for Cargo.toml and other config files
(use-package toml-mode :ensure nil)

(provide 'hc-rust)

;;; hc-rust.el ends here
