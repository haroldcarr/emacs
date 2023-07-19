;;; hc-flycheck --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Jul 18 (Tue) 18:37:51 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(use-package flycheck
  :ensure nil
  :config (global-flycheck-mode 1))

(provide 'hc-flycheck)

;;; hc-flycheck.el ends here

