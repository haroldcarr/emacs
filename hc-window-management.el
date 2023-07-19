;;; hc-window-management.el --- window layout         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package e2wm)
;; M-x e2wm:start-management

(custom-set-faces
 '(e2wm:face-history-list-normal ((t (:foreground "NavajoWhite4")))))

(use-package eyebrowse)

(provide 'hc-window-management)

;;; hc-window-management.el ends here
