;;; hc-rest --- Summary

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package verb)

(use-package org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'hc-rest)

;;; hc-rest ends here
