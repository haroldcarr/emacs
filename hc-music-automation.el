;;; hc-music-automation --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;;;;
;;;; Created       : 2025 Nov 01 (Sat) 20:03:04 by Harold Carr.
;;;; Last Modified : 2025 Dec 25 (Thu) 14:36:42 by Harold Carr.
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
  (hc-play-music hc-music-others '("skylark") '("mp3" "wav" "flac")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HC music

(declare-function hcLocation "")

(defvar hc-music        (hcLocation "music"))
(defvar hc-Carr_tunes   (concat hc-music "/Carr_tunes"))
(defvar hc-2010-Beowawe (concat hc-Carr_tunes "/2010-Beowawe"))
(defvar hc-2010-Georgia (concat hc-Carr_tunes "/2010-Georgia"))

(defun hc-get-tune-names (in substring)
  "Read IN, return lines containing SUBSTRING."
  (let* ((lines (with-temp-buffer
                  (insert-file-contents in)
                  (s-lines (buffer-string))))
         (filtered (-filter
                    (lambda (line) (and (s-contains? substring line)
                                        (s-contains? "./Carr_tunes/" line)))
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

;; after the necessary functions defined
(defvar hc-current-tunes
  (hc-get-tune-names (concat hc-music "/0-AAA-TUNE-LIST.org") "Z25"))

(defun hc-play-zcm-2025 ()
  "Play ZCM 2025."
  (interactive)
  (hc-play-music hc-Carr_tunes hc-current-tunes '("mp3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic

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

(defun hc-play-music (dir-to-search want extensions)
  "Play a tune from a list."
  (interactive)
  (switch-to-buffer "*HC Play It*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let* ((candidates
          (-filter
           (lambda (x) (not (--any? (s-contains? it x) '("Z-SIB"))))
           (directory-files-recursively-with-names-and-extensions dir-to-search want extensions))))

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

(defun hc-quote-if-spaces (s)
  "Return S quoted if it contains whitespace, otherwise return S unchanged."
  (if (string-match-p "[[:space:]]" s)
      (format "%S" s)
    s))

;;(hc-play-it (concat hc-2010-Beowawe "/2023-Beowawe.mp3"))
;;(hc-play-it (concat hc-2010-Georgia "/2010-Georgia.mp3"))

(defun hc-directory-files-recursively-with-extension (dir ext)
  "Return a list of absolute paths for all files under DIR ending with EXT."
  (directory-files-recursively
   dir
   (concat "\\." (regexp-quote ext) "$")))

;;(hc-directory-files-recursively-with-extension hc-Carr_tunes "mp3")
;;(hc-directory-files-recursively-with-extension hc-music-others "m3u")

(defun directory-files-recursively-with-names-and-extensions (dir names exts)
  "Return absolute paths for files under DIR whose basenames contain
any substring in NAMES and whose extensions contain any substring in EXTS.
Matching is case-insensitive."
  (let* ((case-fold-search t)
         (names-re (regexp-opt names))
         (exts-re  (regexp-opt exts))
         (re (concat names-re ".*\\." exts-re "\\'")))
    (directory-files-recursively dir re)))

;;(directory-files-recursively-with-names-and-extensions hc-music-others '("lark" "train") '("mp3" "wav" "flac"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun hc-write-candidate-file (tune-names out)
  "Write lines to OUT after adding header."
  (let* ((with-header (cons "These are candidate tunes.  The list and the tunes themselves are still in progress.\n" tune-names))
         (result (concat (s-join "\n" with-header) "\n")))
    (with-temp-file out (insert result))
    (message "Wrote %d matching lines to %s" (length tune-names) out)))

(provide 'hc-music-automation)

;;; hc-music-automation.el ends here

