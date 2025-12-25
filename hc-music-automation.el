;;; hc-music-automation --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;;;;
;;;; Created       : 2025 Nov 01 (Sat) 20:03:04 by Harold Carr.
;;;; Last Modified : 2025 Dec 25 (Thu) 12:42:26 by Harold Carr.
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HC music

(declare-function hcLocation "")

(defvar hc-music        (hcLocation "music"))
(defvar hc-Carr_tunes   (concat hc-music "/Carr_tunes"))
(defvar hc-2010-Beowawe (concat hc-Carr_tunes "/2010-Beowawe"))
(defvar hc-2010-Georgia (concat hc-Carr_tunes "/2010-Georgia"))
(defvar hc-audacity     "/Applications/Audacity.app/Contents/MacOS/Audacity")
(defvar hc-vlc          "/Applications/VLC.app/Contents/MacOS/VLC")

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
  (hc-play-music hc-current-tunes hc-Carr_tunes "mp3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic

(defvar *hc-play-what*)

(defmacro hc-play-what (what)
  `(progn
     (widget-create 'checkbox
                    :notify #'(lambda (_w &rest _ignore)
                                (setq *hc-play-what* ,what)
                                (message (prin1-to-string *hc-play-what*)))
                    nil)
     (widget-insert (concat " " ,what))))

(defun hc-play-music (want dir-to-search ext)
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
           (-filter
            (lambda (x) (--any? (s-contains? it x) want))
            (hc-directory-files-recursively-with-extension dir-to-search ext)))))

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
  (async-shell-command (concat hc-vlc " --loop " filename) (concat "*" filename "*")))

;;(hc-play-it (concat hc-2010-Beowawe "/2023-Beowawe.mp3"))
;;(hc-play-it (concat hc-2010-Georgia "/2010-Georgia.mp3"))

(defun hc-directory-files-recursively-with-extension (dir ext)
  "Return a list of absolute paths for all files under DIR ending with EXT."
  (directory-files-recursively
   dir
   (concat "\\." (regexp-quote ext) "$")))

;;(hc-directory-files-recursively-with-extension hc-Carr_tunes "mp3")

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

