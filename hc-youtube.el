;;; hc-youtube.el --- youtube          -*- lexical-binding: t; -*-

;;; Commentary:

;; https://xenodium.com/ytr-youtube-radio-for-emacs
;; https://github.com/xenodium/ytr

;;; Code:

(use-package ytr
  :vc (:url "https://github.com/xenodium/ytr" :rev :newest)
  :commands (ytr))

(provide 'hc-youtube)

;;; hc-youtube.el ends here
