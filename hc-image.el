;;; hc-image.el --- image viewing/manipluation         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package eimp
  :init (add-hook 'image-mode-hook 'eimp-mode)
  )

(provide 'hc-image)

;;; hc-image.el ends here
