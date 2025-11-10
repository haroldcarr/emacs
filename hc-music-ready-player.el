;;; hc-music-ready-player.el --- music          -*- lexical-binding: t; -*-

;;; Commentary:

;; https://xenodium.com/bending-emacs-episode-5-ready-player-mode

;;; Code:

(eval-when-compile (require 'use-package))

(use-package ready-player
  :custom
  (ready-player-always-load-directory-recursively nil)
  (ready-player-ask-for-project-sustainability nil)
  (ready-player-autoplay nil)
  (ready-player-hide-modeline nil)
  (ready-player-mode t)
  ;;(ready-player-my-media-collection-location "~/Music/Music/Media.localized/Music")

  ;;:config
  ;;(ready-player-mode +1)
  ;;(set-fontset-font t nil "SF Pro Display" nil 'append)
  ;;(ready-player-macos-use-sf-symbols)
)
