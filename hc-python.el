;;; hc-python --- Summary

;;; Commentary:

;;; Code:

(with-no-warnings
  (use-package geiser
  :ensure t
  :config (progn (setq geiser-racket-binary
                       "/Applications/Racket v6.9/bin/racket")
                 (setq geiser-racket-gracket-binary
                       "/Applications/Racket v6.9/bin/gracket-text")))
  (use-package elpy
    :ensure t
    :init
    (elpy-enable))


  ;; If elpy cannot find the symbol then try a rgrep search.
  ;;
  ;; (defun goto-def-or-rgrep ()
  ;;   "Go to definition of thing at point or do an rgrep in project if that fails"
  ;;   (interactive)
  ;;   (condition-case nil (elpy-goto-definition)
  ;;     (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))

  ;; (define-key elpy-mode-map (kbd "M-.") 'goto-def-or-rgrep)
)

(provide 'hc-python)

;;; hc-python ends here
