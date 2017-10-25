;;; hc-company.el --- completion                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package company
  :ensure t
  :demand
  :diminish ""
  :init
  (progn
    (setq company-idle-delay 0.3)
    (global-company-mode)))

(provide 'hc-company)

;;; hc-company.el ends here
