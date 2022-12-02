;;; hc-haskell-format --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2022 Oct 28 (Fri) 12:16:51 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

(defvar hc-haskell-format-on-save t)

(defvar haskell-stylish-on-save)
(defvar hindent-reformat-buffer-on-save)

(defun hcToggleHaskellFormatOnSave () "."
  (interactive)
  (setq hc-haskell-format-on-save (not hc-haskell-format-on-save))
  (custom-set-variables
   '(haskell-stylish-on-save         hc-haskell-format-on-save)
   '(hindent-reformat-buffer-on-save nil))
  (message "haskell-stylish-on-save: %s; hindent-reformat-buffer-on-save: %s"
           haskell-stylish-on-save      hindent-reformat-buffer-on-save))

(use-package hindent
  :ensure nil
  :config (progn (add-hook 'haskell-mode-hook 'hindent-mode)
                 (custom-set-variables
                  '(hindent-indent-size 4)
                  '(hindent-line-length 120)
                  '(hindent-style "johan-tibell")
                  '(haskell-stylish-on-save hc-haskell-format-on-save)
                  '(hindent-reformat-buffer-on-save nil))))

(provide 'hc-haskell-format)

;;; hc-haskell-format.el ends here

