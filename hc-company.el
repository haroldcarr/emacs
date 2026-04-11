;;; hc-company.el --- completion                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package company
  :ensure nil
  :demand
  :diminish ""
  :init
  (setq company-idle-delay 0.3)
  :config
  (setq company-backends '(company-capf))
  (global-company-mode)
)

(provide 'hc-company)

;;; hc-company.el ends here
