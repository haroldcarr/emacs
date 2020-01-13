;;; hc-haskell-dante --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2020 Jan 13 (Mon) 10:21:02 by Harold Carr.
;;;;

;;; Code:

;;; Dante

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (progn
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'haskell-mode-hook 'flycheck-mode))
  :diminish " λ"
  :config
  (progn
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
    ;; https://github.com/shajra/example-nix
    ;;(setq dante-repl-command-line-methods-alist
    ;;      (cons `(nix-new . ,(lambda (root)
    ;;                           (dante-repl-by-file
    ;;                            (projectile-project-root)
    ;;                            '("shell.nix")
    ;;                            `("nix-shell" "--run" "cabal new-repl"
    ;;                              ,(concat (projectile-project-root) "/shell.nix")))))
    ;;            dante-repl-command-line-methods-alist))
    ;; HC: FOLLOW INTERO KEY BINDINGS
    (define-key dante-mode-map (kbd "C-c C-t") 'dante-type-at)
    (define-key dante-mode-map (kbd "C-c C-i") 'dante-info)
  )
  :custom
  (dante-methods-alist
   `((stack "stack.yaml" ("stack" "repl" dante-target))
     (styx "styx.yaml" ("styx" "repl" dante-target))
    ; (snack ,(lambda (d) (directory-files d t "package\\.\\(yaml\\|nix\\)")) ("snack" "ghci" dante-target)) ; too easy to trigger, confuses too many people.
     (new-impure-nix dante-cabal-new-nix ("nix-shell" "--run" (concat "cabal new-repl " (or dante-target (dante-package-name) "") " --builddir=dist/dante")))
     (new-nix dante-cabal-new-nix ("nix-shell" "--pure" "--run" (concat "cabal new-repl " (or dante-target (dante-package-name) "") " --builddir=dist/dante")))
     (nix dante-cabal-nix ("nix-shell" "--pure" "--run" (concat "cabal repl " (or dante-target "") " --builddir=dist/dante")))
     (impure-nix dante-cabal-nix ("nix-shell" "--run" (concat "cabal repl " (or dante-target "") " --builddir=dist/dante")))
     (new-build "cabal.project.local" ("cabal" "new-repl" (or dante-target (dante-package-name) nil) "--builddir=dist/dante"))
     (nix-ghci ,(lambda (d) (directory-files d t "shell.nix\\|default.nix")) ("nix-shell" "--pure" "--run" "ghci"))
     (mafia "mafia" ("mafia" "repl" dante-target))
     (bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "repl" dante-target "--builddir=dist/dante"))
     (bare-ghci ,(lambda (_) t) ("ghci"))))
)

(provide 'hc-haskell-dante)

;;; hc-haskell-dante.el ends here

