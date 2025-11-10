;;; hc-music-emms.el --- music          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'hc-music-volume)

(eval-when-compile (require 'use-package))

;; https://www.gnu.org/software/emms/manual/
(use-package emms-setup
  :custom
  (emms-source-file-default-directory (concat (hcUlhcd) "/00-music-root"))
  :config
  (emms-all)
  (emms-default-players)
  (emms-browser-add-category "title" 'info-title)
  :bind (:map emms-browser-mode-map
              ("b 7" .'emms-browse-by-title))
)

;; brew install exiftool
(require 'emms-info-exiftool)
(add-to-list 'emms-info-functions 'emms-info-exiftool)

(require 'emms-history)
(emms-history-load)

;; HC: not sure if this is necessary
;; https://osxdaily.com/2018/04/29/play-flac-audio-files-mac-vlc/
;; brew install vlc

;; https://www.emacswiki.org/emacs/EMMS
;; Using EMMS on OSX without installing extra audio players
(define-emms-simple-player afplay '(file)
  (regexp-opt '(".mp3" ".m4a" ".aac" ".flac")) "afplay")

(setq emms-player-list `(,emms-player-vlc)) ;; ,emms-player-afplay))

;; read and cache tags of all music files
;; M-x emms-add-directory-tree RET emms-source-file-default-directory
;; (emms-add-directory-tree emms-source-file-default-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emms-volume-change-function 'hc-mac-volume-change)
(setq emms-volume-change-amount 10)

(provide 'hc-music-emms)

;;; hc-music-emms.el ends here
