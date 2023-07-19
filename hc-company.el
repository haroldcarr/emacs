;;; hc-company.el --- completion                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package company
  :ensure nil
  :demand
  :diminish ""
  :init
  (progn
    (setq company-idle-delay 0.3)
    (global-company-mode)))

(provide 'hc-company)

;;; hc-company.el ends here
