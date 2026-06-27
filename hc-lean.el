;;; hc-lean --- Summary                      -*- lexical-binding: t; -*-

;;; Commentary:

;; ...

;;;;
;;;; Created       : 2026 Jun 23 (Tue) 17:38:11 by Harold Carr.
;;;; Last Modified : 2026 Jun 23 (Tue) 18:59:05 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

;; M-x package-vc-install
;; RET https://github.com/leanprover-community/lean4-mode

(use-package lean4-mode
  :mode "\\.lean\\'")

(provide 'hc-lean)

;;; hc-lean.el ends here
