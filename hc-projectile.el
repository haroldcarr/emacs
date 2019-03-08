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
            (t (projectile-test-suffix project-type))))

    (setq projectile-create-missing-test-files t
          projectile-mode-line                 nil
          projectile-test-suffix-function      #'init-projectile-test-suffix
          projectile-use-git-grep              t)
    (make-variable-buffer-local 'projectile-tags-command)
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (projectile-mode)))

(provide 'hc-projectile)

;;; hc-projectile.el ends here
