;;; hc-haskell-dante --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2018 Oct 04 (Thu) 15:38:00 by Harold Carr.
;;;;

;;; Code:

;;; Dante

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  :diminish " λ"
  :config
  (progn
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
    ;; https://github.com/shajra/example-nix
    (setq dante-repl-command-line-methods-alist
          (cons `(nix-new . ,(lambda (root)
                               (dante-repl-by-file
                                (projectile-project-root)
                                '("shell.nix")
                                `("nix-shell" "--run" "cabal new-repl"
                                  ,(concat (projectile-project-root) "/shell.nix")))))
                dante-repl-command-line-methods-alist))
    ))


(provide 'hc-haskell-dante)

;;; hc-haskell-dante.el ends here

