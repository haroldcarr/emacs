;;; hc-web-mode.el --- web-mode          -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(eval-when-compile (require 'use-package))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2))

(provide 'hc-web-mode)

;;; hc-web-mode.el ends here
