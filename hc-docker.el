;;; hc-docker.el --- docker          -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(eval-when-compile (require 'use-package))

;; https://github.com/Silex/docker.el
(use-package docker
  :bind ("C-c d" . docker))

(provide 'hc-docker)

;;; hc-docker.el ends here
