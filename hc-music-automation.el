;;; hc-music-automation --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;;;;
;;;; Created       : 2025 Nov 01 (Sat) 20:03:04 by Harold Carr.
;;;; Last Modified : 2026 May 17 (Sun) 16:49:05 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'hc-run-command)
(require 'hc-ssh)
(require 's)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(eval-when-compile
  (require 'wid-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; play along

(defvar hc-music-others "/Volumes/exFAT-exter/MusicOthers")

(defun hc-play-play-along ()
  "Play play along."
  (interactive)
  (hc-play-music
   "PLAY ALONG"
   hc-music-others
   '(
     "airegin"
     "all the things you are"
     "Body and Soul" "Body & Soul"
     "bolivia"
     "Fee-Fi-Fo-Fum" "Fee Fi Fo Fum"
     "foolish dog" "foolish door"
     "round midnight" "round about midnight"
     "skylark"
     )
   '(
     "199x-Stray"
     "2009-12-22-rehearsal"
     "2010-01-04-rehearsal"
     "2010-01-18-rehearsal"
     "2010-01-20-City-Art"
     "2010-07-14-Wayne"
     "2010-10-26-John-Stowell"
     "2021-spring"
     "2022-05-07-Foolish_Door"
     "2022-06-02-Emilee"
     "2022-07-28-Emilee"
     "2022-08-18-keith"
     "2022-10-03-goran"
     "2022-10-08-goran"
     "2022-10-28-flanders"
     "2024-06-01-tony-elison"
     "2024-08-29-josh"
     "._Foolish_Door"
     )
   '("flac" "m4a" "mp3" "wav")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HC music

(declare-function hcLocation "")

(defvar hc-music             (hcLocation           "music"))
(defvar hc-Carr_tunes        (concat hc-music      "/Carr_tunes"))
(defvar hc-Carr_arrangements (concat hc-music      "/Carr_arrangements"))
(defun  hc-zcm-2025-tunes () (hc-get-tune-names (concat hc-music "/0-AAA-TUNE-LIST.org") "Z25"))

(defun hc-get-tune-names (in substring)
  "Read IN, return lines containing SUBSTRING."
  (let* ((lines (with-temp-buffer
                  (insert-file-contents in)
                  (s-lines (buffer-string))))
         (filtered (-filter
                    (lambda (line) (and (s-contains? substring line)
                                        (or (s-contains? "./Carr_arrangements/"     line)
                                            (s-contains? "./Carr_book/"             line)
                                            (s-contains? "./Carr_practice-loops/"   line)
                                            (s-contains? "./Carr_tunes/"            line)
                                            (s-contains? "./Carr_tunes_unfinished/" line))))
                    lines)))
    (mapcar #'hc-extract-tune-name filtered)))

;;(hc-get-tune-names (concat hc-music "/0-AAA-TUNE-LIST.org") "Z25")
;;(hc-write-candidate-file
;; (hc-get-tune-names (concat hc-music "/0-AAA-TUNE-LIST.org") "Z25")
;; "/tmp/XXX")

(defun hc-extract-tune-name (line)
  "From a LINE like \"./Carr_tunes/1976-Transformation/ ...\",
return \"1976-Transformation\"."
  (->> line
       (s-split "/")                             ; split on slashes
       (-filter (lambda (x) (not (s-blank? x)))) ; drop empty lines
       (-third-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HAJJ

(defun hajj () (tagged-with "HAJJ" hc-Carr_tunes))
(defun solo ()
  (tagged-with "SOLO" hc-Carr_arrangements)
  (tagged-with "SOLO" hc-Carr_tunes))

(defun tagged-with (tag-string dir-to-search)
  (interactive)
  (-each (hc-get-tune-names (concat hc-music "/0-AAA-TUNE-LIST.org") tag-string)
    #'(lambda (tune)
        (princ (format "\n----------------------------------------------------------------------------\n"))
        (princ (format "%s\n\n" tune))
        (-each
            (directory-files-recursively-with-want-and-do-not-want-and-extensions
             dir-to-search
             (list tune) ;; want
             '("ZZZ" "Violin" "melody" "Z-SIB" "Piano"
               "sketch" "piano" "Vibraphone" "Trombone") ;; do not want
             '("jpg" "jpeg" "mp3" "mscz" "sib" "pdf")) ;; extensions
          #'(lambda (x) (princ (format "%s\n" x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO : replace with EMMS

(defun hc-play-zcm-2025 ()
  "Play ZCM 2025."
  (interactive)
  (hc-play-music "ZCM 2025" hc-Carr_tunes (hc-zcm-2025-tunes) '("Z-SIB") '("mp3")))

(defvar hc-audacity "/Applications/Audacity.app/Contents/MacOS/Audacity")
(defvar hc-vlc      "/Applications/VLC.app/Contents/MacOS/VLC")

(defvar *hc-play-what*)

(defmacro hc-play-what (what)
  `(progn
     (widget-create 'checkbox
                    :notify #'(lambda (_w &rest _ignore)
                                (setq *hc-play-what* ,what)
                                (message (prin1-to-string *hc-play-what*)))
                    nil)
     (widget-insert (concat " " ,what))))

(defun hc-play-music (playlist-name dir-to-search want do-not-want extensions)
  "Play a tune from a list."
  (interactive)
  (switch-to-buffer (concat "*" playlist-name "*"))
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let* ((candidates
          (directory-files-recursively-with-want-and-do-not-want-and-extensions
           dir-to-search want do-not-want extensions)))

    ;; --------------------------------------------------
    (-each candidates
      #'(lambda (c)
          (widget-insert "\n")
          (hc-play-what c)))

    ;; --------------------------------------------------
    (widget-insert "\n\n")
    (widget-create
     'push-button
     :notify
       #'(lambda (&rest _ignore)
           (message *hc-play-what*)
           (hc-play-it *hc-play-what*))
     "play")

    ;; --------------------------------------------------
    (widget-insert "\n")

    ;; --------------------------------------------------
    (use-local-map widget-keymap)
    (widget-setup)))

(defun hc-play-it (filename)
  (async-shell-command
   (concat hc-vlc " --loop " (hc-quote-if-spaces filename))
   (concat "*" filename "*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun directory-files-recursively-with-want-and-do-not-want-and-extensions (dir-to-search want do-not-want exts)
  (-filter
   (lambda (x) (not (--any? (s-contains? it x) do-not-want)))
   (directory-files-recursively-with-want-and-extensions dir-to-search want exts)))

(defun directory-files-recursively-with-want-and-extensions (dir names exts)
  "Return absolute paths for files under DIR whose names contain
any substring in NAMES and whose extensions contain any substring in EXTS.
Matching is case-insensitive."
  (let* ((case-fold-search t)
         (names-re         (regexp-opt names))
         (exts-re          (regexp-opt exts))
         (re               (concat names-re ".*\\." exts-re "\\'")))
    (directory-files-recursively dir re nil #'file-directory-p t)))

;;(directory-files-recursively-with-want-and-extensions hc-music-others '("lark" "train") '("mp3" "wav" "flac"))

(defun hc-write-candidate-file (tune-names out)
  "Write lines to OUT after adding header."
  (let* ((with-header (cons "These are candidate tunes.  The list and the tunes themselves are still in progress.\n" tune-names))
         (result (concat (s-join "\n" with-header) "\n")))
    (with-temp-file out (insert result))
    (message "Wrote %d matching lines to %s" (length tune-names) out)))

(defun hc-quote-if-spaces (s)
  "Return S quoted if it contains whitespace, otherwise return S unchanged."
  (if (string-match-p "[[:space:]]" s)
      (format "%S" s)
    s))

(provide 'hc-music-automation)

;;; hc-music-automation.el ends here

