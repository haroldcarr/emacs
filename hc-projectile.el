;;; hc-projectile --- init file         -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(require 'tramp)

(use-package projectile
  :demand
  :diminish ""
  :init
  (progn
    (defun init-projectile-test-suffix (project-type)
      "Find default test files suffix based on PROJECT-TYPE."
      (cond ((member project-type '(haskell-stack)) "Spec")
            ((member project-type '(haskell-cabal)) "Spec")
            (t (projectile-test-suffix project-type))))

    (setq projectile-create-missing-test-files t
          projectile-mode-line                 nil
          projectile-test-suffix-function      #'init-projectile-test-suffix
          projectile-use-git-grep              t)
    (make-variable-buffer-local 'projectile-tags-command)
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (projectile-mode))
  :config
    ;; When 'default.nix' and '*.cabal' in same directory, projectile thinks it is a 'nix' project.
    ;; The default for 'nix' does not specify a ':test-suffix', so specify my choice here.
    (projectile-update-project-type
     'nix
      :test-suffix "Spec")
    ;; these do not work
    ;;(push "Setup.hs"          projectile-globally-ignored-files)
    ;;(push "^\\dist-newstyle$" projectile-globally-ignored-directories)
                            ;;projectile-globally-ignored-file-suffixes
                            ;;projectile-globally-ignored-modes
)
(provide 'hc-projectile)

;;; hc-projectile.el ends here
