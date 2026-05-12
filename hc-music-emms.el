;;; hc-music-emms.el --- music          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package emms)

(require 'emms-setup)
(emms-all)
(setq emms-player-list    '(emms-player-mpv))
(setq emms-info-functions '(emms-info-native))

(require 'emms-mode-line)
(require 'emms-playing-time)
(emms-mode-line 1)
(emms-playing-time 1)

(require 'hc-music-volume)
(setq emms-volume-change-function 'hc-mac-volume-change)
(setq emms-volume-change-amount 10)

(emms-add-directory "/usr/local/hc/00-music-with-friends/00-with-friends/2025-ZCM/ZCM25_MP3")
(emms-add-directory "/usr/local/hc/00-music-with-friends/00-with-friends/2025-ZCM/2026-03-26-mixes")

;; ------------------------------------------------------------------------------
;; EMMS playlist highlight config

(require 'emms-playlist-mode)

;; When jumping to current track, center it
(setq emms-playlist-mode-center-when-go t)

;; Highlight the currently selected/playing track
(custom-set-faces
 '(emms-playlist-selected-face
   ((t (:inherit highlight :weight bold)))))

;; Ensure playlist follows current track when playback starts
(add-hook 'emms-player-started-hook
          #'emms-playlist-mode-center-current)

;; Notes:
;; - emms-playlist-selected-face is used for the current track
;; - This highlight should persist when paused/stopped
;; - If not visible, customize the face manually:
;;     M-x customize-face RET emms-playlist-selected-face RET

;; ------------------------------------------------------------------------------
;; custom EMMS track display

;; - short entries: show full
;; - long paths: trim LEFT

(defun hc-emms-track-description (track)
  (let ((info (emms-info-track-description track))
        (max  65))
    (if (<= (length info) max)
        info
      (concat "…" (substring info (- (length info) max))))))

(setq emms-track-description-function #'hc-emms-track-description)

;; ------------------------------------------------------------------------------
;; OLD
;; (eval-when-compile (require 'use-package))

;; ;; https://www.gnu.org/software/emms/manual/
;; (use-package emms-setup
;;   :custom
;;   (emms-source-file-default-directory (concat (hcUlhcd) "/00-music-root"))
;;   :config
;;   (emms-all)
;;   (emms-default-players)
;;   (emms-browser-add-category "title" 'info-title)
;;   :bind (:map emms-browser-mode-map
;;               ("b 7" .'emms-browse-by-title))
;; )

;; ;; brew install exiftool
;; (require 'emms-info-exiftool)
;; (add-to-list 'emms-info-functions 'emms-info-exiftool)

;; (require 'emms-history)
;; (emms-history-load)

;; ;; HC: not sure if this is necessary
;; ;; https://osxdaily.com/2018/04/29/play-flac-audio-files-mac-vlc/
;; ;; brew install vlc

;; ;; https://www.emacswiki.org/emacs/EMMS
;; ;; Using EMMS on OSX without installing extra audio players
;; (define-emms-simple-player afplay '(file)
;;   (regexp-opt '(".mp3" ".m4a" ".aac" ".flac")) "afplay")

;; (setq emms-player-list `(,emms-player-vlc)) ;; ,emms-player-afplay))

;; ;; read and cache tags of all music files
;; ;; M-x emms-add-directory-tree RET emms-source-file-default-directory
;; ;; (emms-add-directory-tree emms-source-file-default-directory)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hc-music-emms)

;;; hc-music-emms.el ends here
