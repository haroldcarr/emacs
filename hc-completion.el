;;; hc-completion.el --- completion                     -*- lexical-binding: t; -*-

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

(provide 'hc-completion)

;;; hc-completion.el ends here
