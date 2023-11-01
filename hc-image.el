;;; hc-image.el --- image viewing/manipluation         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;;
;; EIMP : interactive image manipulation from within Emacs.
;; Uses mogrify utility from ImageMagick to do transformations.
;;
;; Turn minor mode on with
;;
;;     (eimp-mode 1)
;;
;; or toggle with M-x eimp-mode RET.
;; (use-package eimp
;;   :init (add-hook 'image-mode-hook 'eimp-mode)
;;   )

(use-package image
  :custom
  ;; Enable converting external formats (ie. webp) to internal ones.
  ;; Need to have imagemagick install
  (image-use-external-converter t))

(provide 'hc-image)

;;; hc-image.el ends here
