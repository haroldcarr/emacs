;;; hc-lsp-pick --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Mar 10 (Fri) 10:58:47 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

(defvar hc-haskell-pick)
(defvar hc-rust-pick)

(defun hc-pick-lsp-support ()
  "Prompt pick from a list."
  (interactive)
  (let ((choices '("eglot" "lsp" "none")))
    (message "%s" (ido-completing-read "lsp?: " choices))))

(let ((pick (hc-pick-lsp-support)))
  (cond (;; -------------------------
         (equal pick "eglot")
         (message "Using hc-eglot-haskell")
         (use-package hc-eglot-haskell)
         (setq hc-haskell-pick 'hc-eglot-haskell)
         )
        (;; -------------------------
         (equal pick "lsp")
         (message "Using hc-lsp-haskell")
         (use-package hc-lsp-haskell)
         (setq hc-haskell-pick 'hc-lsp-haskell)
         (message "Using hc-lsp-rust")
         (use-package hc-lsp-rust)
         (setq hc-rust-pick    'hc-lsp-rust)
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

