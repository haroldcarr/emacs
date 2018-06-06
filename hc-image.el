;;; hc-image.el --- image viewing/manipluation         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eimp
  :init (add-hook 'image-mode-hook 'eimp-mode)
  )

(provide 'hc-image)

;;; hc-image.el ends here
