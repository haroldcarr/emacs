;;; hc-lsp-pick --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Aug 17 (Thu) 09:13:03 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

;;; Haskell Packages

(defvar hc-haskell-pick)
(defvar hc-rust-pick)

(defun hc-pick-lsp-support ()
  "Prompt pick from a list."
  (interactive)
  (let ((choices '("eglot" "lsp" "none")))
    (message "%s" (ido-completing-read "IDE pick: " choices))))

(defun hc-use-hc-lsp-rust ()
  "DRY."
  (message "Using hc-lsp-rust")
  (use-package hc-lsp-rust)
  (setq hc-rust-pick 'hc-lsp-rust))

(let ((pick (hc-pick-lsp-support)))
  (cond (;; -------------------------
         (equal pick "eglot")
         (message "Using hc-eglot-haskell")
         (use-package hc-eglot-haskell)
         (setq hc-haskell-pick 'hc-eglot-haskell)

         (hc-use-hc-lsp-rust) ;; TODO: eglot-rust
         )

        (;; -------------------------
         (equal pick "lsp")
         (message "Using hc-lsp-haskell")
         (use-package hc-lsp-haskell)
         (setq hc-haskell-pick 'hc-lsp-haskell)

         (hc-use-hc-lsp-rust)
        )

        (;; -------------------------
         (equal pick "none")
         (message "NO LSP SUPPORT")
         (setq hc-haskell-pick 'none)
         (setq hc-rust-pick    'none)
        )

        (t
         (message "NO LSP MATCH %s" pick))))

;;;;;;;;;;;;;

(provide 'hc-lsp-pick)

;;; hc-lsp-pick.el ends here

