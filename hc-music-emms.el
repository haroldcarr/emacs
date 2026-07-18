;;; hc-music-emms.el --- music          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package emms
  :config
  ;; by default, emms names this with a leading space: " *EMMS Playlist*"
  ;; which causes emms playlist buffers not to show in a buffer list.
  (setopt emms-playlist-buffer-name "*EMMS Playlist*"))

(require 'emms-setup)
(emms-all)

(require 'emms-player-simple)
(require 'emms-player-mpv)
(require 'emms-player-vlc)
(setq emms-player-vlc-command-name "/Applications/VLC.app/Contents/MacOS/VLC")

;; on o2011 : older version
;; nix shell github:NixOS/nixpkgs/nixos-23.05#mpv
(defun hc-emms-use-mpv ()
  (interactive)
  (setq emms-player-list '(emms-player-mpv)))

(defun hc-emms-use-vlc ()
  (interactive)
  (setq emms-player-list           '(emms-player-vlc))
  (setq emms-player-vlc-parameters '("--intf" "macosx")))

;; M-x hc-emms-use-mpv   ;; headless mpv
;; M-x hc-emms-use-vlc   ;; GUI VLC

(hc-emms-use-vlc)

(setq emms-info-functions '(emms-info-native))

(require 'emms-mode-line)
(emms-mode-line-mode 1)

(require 'emms-playing-time)
(emms-playing-time-mode 1)

(require 'emms-volume)
(require 'hc-music-volume)
(setq emms-volume-change-function 'hc-mac-volume-change)
(setq emms-volume-change-amount 10)

;;(emms-add-directory "/usr/local/hc/00-music-with-friends/00-with-friends/2025-ZCM/ZCM25_MP3")
;;(emms-add-directory "/usr/local/hc/00-music-with-friends/00-with-friends/2025-ZCM/2026-03-26-mixes")

;; ------------------------------------------------------------------------------
;; track display

(defun hc-emms-track-description (track)
  (let ((name        (emms-track-get track 'name))
        (title       (emms-track-get track 'info-title))
        (album       (emms-track-get track 'info-album))
        (albumartist (emms-track-get track 'info-albumartist))
        (artist      (emms-track-get track 'info-artist)))
    (concat
     (or title (hc-last-n-chars name 40))
     ":"
     (or albumartist artist)
     ":"
     album
     )))

(defun hc-last-n-chars (s n)
  (substring s (- (length s) (min n (length s)))))

(setq emms-track-description-function #'hc-emms-track-description)

(defun hc-emms-refresh-playlist-buffer ()
  "Refresh the visible EMMS playlist buffer."
  (interactive)
  (with-current-emms-playlist
    (let ((inhibit-read-only t)
          (tracks (emms-playlist-tracks-in-region (point-min) (point-max))))
      (erase-buffer)
      (mapc #'emms-playlist-mode-insert-track tracks)
      (goto-char (point-min)))))

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

(defun hc-emms-play-file-at-point ()
  "Play file path at point with EMMS."
  (interactive)
  (let ((f (thing-at-point 'filename t)))
    (unless f
      (user-error "No filename at point"))
    (setq f (expand-file-name f))
    (unless (file-exists-p f)
      (user-error "File does not exist: %s" f))
    (emms-play-file f)))

;;(global-set-key (kbd "C-c e p") #'hc-emms-play-file-at-point)

(defun hc-emms-add-file-at-point ()
  "Add file at point to EMMS playlist."
  (interactive)
  (let ((f (thing-at-point 'filename t)))
    (unless f (user-error "No filename at point"))
    (emms-add-file (expand-file-name f))))

;;(global-set-key (kbd "C-c e a") #'hc-emms-add-file-at-point)

;; ------------------------------------------------------------------------------
;; add playable paths in buffer to EMMS
;; for use after hc-music-automation (hajj)

(defvar hc-emms-playable-extensions
  '("mp3" "flac" "wav" "m4a" "aac" "ogg" "opus" "aiff" "aif")
  "File extensions considered playable by EMMS.")

(defun hc-emms-playable-file-p (path)
  "Return non-nil if PATH looks like a playable existing file."
  (and path
       (file-exists-p path)
       (member (downcase (or (file-name-extension path) ""))
               hc-emms-playable-extensions)))

(defun hc-emms-buffer-playable-files ()
  "Return playable file paths from current buffer, top to bottom."
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^/.*$" nil t)
        (let ((path (string-trim (match-string-no-properties 0))))
          (when (hc-emms-playable-file-p path)
            (push path files)))))
    (nreverse files)))

(defun hc-emms-add-buffer-tracks ()
  "Add all playable file paths in current buffer to EMMS playlist."
  (interactive)
  (let ((files (hc-emms-buffer-playable-files)))
    (unless files
      (user-error "No playable tracks found"))
    (dolist (file files)
      (emms-add-file file))
    (message "Added %d tracks to EMMS playlist" (length files))))

;; optional keybinding
;; (global-set-key (kbd "C-c e b") #'hc-emms-add-buffer-tracks)

;; ------------------------------------------------------------------------------
;; YouTube EXTM3U for EMMS/mpv

;; USAGE:
;;
;; Get YT topic share URL
;; cd ~/z-TMP-COPY/yt-search-m3u
;; cargo run -- "Tethered Moon Experiencing Tosca" experiencing-tosca.m3u
;; (hc-emms-play-youtube-m3u "~/z-TMP-COPY/yt-search-m3u/experiencing-tosca.m3u")

;; summary
;; - Playable thing = (emms-track 'url URL)
;; - Display name = info-title (NOT name)
;; - Do NOT use emms-play-playlist for YouTube M3U
;; - Build tracks yourself → insert → play

(require 'emms)
(require 'emms-player-mpv)
(require 'emms-playlist-mode)
(require 'subr-x)

;;(setq emms-player-list '(emms-player-mpv))

(defun hc-emms-youtube-m3u--tracks (file)
  "Return EMMS tracks from YouTube EXTM3U FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((title nil)
          tracks)
      (dolist (raw-line (split-string (buffer-string) "\n" t))
        (let ((line (string-trim raw-line)))
          (cond
           ((string-match "\\`#EXTINF:[^,]*,\\(.*\\)\\'" line)
            (setq title (match-string 1 line)))

           ((or (string-empty-p line)
                (string-prefix-p "#" line))
            nil)

           (t
            (let ((track (emms-track 'url line)))
              ;; Keep URL as playable name.
              ;; Store title only as display metadata.
              (when title
                (emms-track-set track 'info-title title))
              (push track tracks))
            (setq title nil)))))
      (nreverse tracks))))

(defun hc-emms-add-youtube-m3u (file)
  "Add YouTube EXTM3U FILE to a fresh EMMS playlist."
  (interactive "fM3U file: ")
  (let ((tracks (hc-emms-youtube-m3u--tracks file))
        (buf (emms-playlist-new "youtube-m3u")))
    (emms-playlist-set-playlist-buffer buf)
    (with-current-buffer buf
      (dolist (track tracks)
        (emms-playlist-insert-track track))
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun hc-emms-play-youtube-m3u (file)
  "Load YouTube EXTM3U FILE into EMMS and play first track."
  (interactive "fM3U file: ")
  (hc-emms-add-youtube-m3u file)
  (with-current-buffer emms-playlist-buffer
    (goto-char (point-min))
    (emms-playlist-select (point))))

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
