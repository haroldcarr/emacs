;;; hc-youtube.el --- youtube          -*- lexical-binding: t; -*-

;;; Commentary:

;; https://xenodium.com/ytr-youtube-radio-for-emacs
;; https://github.com/xenodium/ytr

;;; Code:

;; This one does not need a login.
;; Just give it youtube URLs.
(use-package ytr
  :vc (:url "https://github.com/xenodium/ytr" :rev :newest)
  :commands (ytr))


;;------------------------------------------------------------------------------

;; This one has you login.
;; Then it shows/plays your home, playlists, likes.

(use-package youtube-music
  :ensure nil
  :bind ("C-c y" . youtube-music)
  :config (youtube-music-modeline-mode 1))

(provide 'hc-youtube)

;;; hc-youtube.el ends here
