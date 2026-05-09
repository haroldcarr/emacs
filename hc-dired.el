;;; hc-dired.el --- Summary dired

;;; Commentary:

;;; Code:

(use-package all-the-icons-dired
  ;; M-x all-the-icons-install-fonts
  :ensure nil
  :commands (all-the-icons-dired-mode)
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hc-dired)

;;; hc-dired.el ends here
