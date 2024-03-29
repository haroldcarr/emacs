;;; hc-python --- Summary

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package python-mode
  :ensure nil)

(use-package elpy
  :ensure nil
  :defer  t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

  ;; If elpy cannot find the symbol then try a rgrep search.
  ;;
  ;; (defun goto-def-or-rgrep ()
  ;;   "Go to definition of thing at point or do an rgrep in project if that fails"
  ;;   (interactive)
  ;;   (condition-case nil (elpy-goto-definition)
  ;;     (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))

  ;; (define-key elpy-mode-map (kbd "M-.") 'goto-def-or-rgrep)

(provide 'hc-python)

;;; hc-python ends here
