;;; hc-projectile --- init file         -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(require 'tramp)

(use-package projectile
  :demand
  :diminish ""
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-create-missing-test-files t
        projectile-mode-line                 nil
        projectile-use-git-grep              t)
  (make-variable-buffer-local 'projectile-tags-command)
  (projectile-mode)
  :config
  ;; HACK to get haskell-cabal near the beginning so it is discovered
  ;; before nix, Make, stack, etc.
  ;; TODO : make this deterministic.
  (setq projectile-project-types (reverse projectile-project-types))
)
(provide 'hc-projectile)

;;; hc-projectile.el ends here
