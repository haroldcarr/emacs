;;; hc-dashboard.el --- dashboard          -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

;; https://github.com/rakanalh/emacs-dashboard

(with-no-warnings
(use-package dashboard
   :config
   (progn
     (dashboard-setup-startup-hook)
     (setq dashboard-banner-logo-title "Emacs Dashboard")
     (setq dashboard-startup-banner nil)
     (setq dashboard-items '((recents   . 10)
                             (projects  .  8)
                             (bookmarks .  2)
                             ;;(agenda    . 5)
                             ;;(registers . 5)
                             ))
     ))
)

(defun hc-dashboard () ""
  (dashboard-insert-startupify-lists)
  (switch-to-buffer "*dashboard*")
  (goto-char (point-min))
  (redisplay))

(hc-dashboard)

(provide 'hc-dashboard)

;;; hc-dashboard.el ends here
