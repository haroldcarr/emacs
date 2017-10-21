;;; hc-haskell-intero --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2017 Oct 19 (Thu) 10:57:59 by Harold Carr.
;;;;

;;; Code:

;;; Intero

(use-package intero
  :ensure t
  :defer t
  :diminish " λ"
  :bind (:map intero-mode-map
              ("M-." . init-intero-goto-definition))
  :init
  (progn
    (defun hc-init-intero ()
      "Enable Intero unless visiting a cached dependency."
      (interactive)
      (cond ((and buffer-file-name
                  (string-match ".+/\\.\\(stack\\|stack-work\\)/.+" buffer-file-name))
             (eldoc-mode -1)
             (flycheck-mode -1))
            (t
             ;; https://github.com/commercialhaskell/intero/issues/208
             (setq flycheck-check-syntax-automatically '(mode-enabled save))
             (intero-mode)
             (setq projectile-tags-command "codex update"))))
    (defun hc-add-intero-to-haskell-mode-hook ()
      "Do an explicit call to avoid automatic download when not installed."
      (interactive)
      (add-hook 'haskell-mode-hook #'hc-init-intero))
  )
  :config
  (progn
    (defun init-intero-goto-definition ()
      "Jump to the definition of the thing at point using Intero or etags."
      (interactive)
      (or (intero-goto-definition)
          (find-tag (find-tag-default))))

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))

(provide 'hc-haskell-intero)

;;; hc-haskell-intero.el ends here

