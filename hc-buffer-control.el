;;; hc-buffer-control --- for emacs --- lexical-binding   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'display-buffer-alist
             '("\\*Flycheck errors\\**"
               (display-buffer-in-direction)
               (direction . bottom-only)))

(provide 'hc-buffer-control)

;;; hc-buffer-control.el ends here

