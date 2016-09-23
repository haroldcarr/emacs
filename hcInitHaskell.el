;;; hcInitHaskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2016 Sep 23 (Fri) 11:02:09 by Harold Carr.
;;;;

;;; Code:

;; COMMON
(defvar hc-haskell-format-on-save nil) ;; nil turns to t when loaded

(defun hc-toggle-haskell-format-on-save ()
  (interactive)
  (setq hc-haskell-format-on-save (not hc-haskell-format-on-save))
  (custom-set-variables
   '(haskell-stylish-on-save hc-haskell-format-on-save)
   '(hindent-reformat-buffer-on-save hc-haskell-format-on-save))
  (message "save is: %s" hc-haskell-format-on-save))

(use-package hindent
  :config (progn (add-hook 'haskell-mode-hook 'hindent-mode)
                 (custom-set-variables
                  '(hindent-indent-size 4)
                  '(hindent-line-length 120)
                  '(hindent-style "johan-tibell"))
                 (hc-toggle-haskell-format-on-save)))

;; ------------------------------------------------------------------------------

(use-package intero
  :config (progn (add-hook 'haskell-mode-hook 'intero-mode)
                 ;; https://github.com/commercialhaskell/intero/issues/208
                 (setq flycheck-check-syntax-automatically '(mode-enabled save))))

(provide 'hcInitHaskell)

;;; hcInitHaskell.el ends here

