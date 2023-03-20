;;; hc-dired.el --- Summry dired

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.n16f.net/blog/decluttering-dired-for-peace-of-mind/

(defun g-dired-postprocess-ls-output ()
  "Postprocess the list of files printed by the ls program when
executed by Dired."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      ;; Go to the beginning of the next line representing a file
      (while (null (dired-get-filename nil t))
        (dired-next-line 1))
      (beginning-of-line)
      ;; Narrow to the line and process it
      (let ((start (line-beginning-position))
            (end (line-end-position)))
        (save-restriction
          (narrow-to-region start end)
          (setq inhibit-read-only t)
          (unwind-protect
              (g-dired-postprocess-ls-line)
            (setq inhibit-read-only nil))))
      ;; Next line
      (dired-next-line 1))))

(defun g-dired-disable-line-wrapping ()
  (setq truncate-lines t))

(defun g-dired-postprocess-ls-line ()
  "Postprocess a single line in the ls output, i.e. the information
about a single file. This function is called with the buffer
narrowed to the line."
  ;; Highlight everything but the filename
  (when (re-search-forward directory-listing-before-filename-regexp nil t 1)
    (add-text-properties (point-min) (match-end 0) '(font-lock-face shadow)))
  ;; Hide the link count
  (beginning-of-line)
  (when (re-search-forward " +[0-9]+" nil t 1)
    (add-text-properties (match-beginning 0) (match-end 0) '(invisible t))))

(when (member (hcMachineName) '()) ;;'("o2020"))
  (use-package dired
    :config
    (setq dired-listing-switches "-alh --time-style=long-iso")
    :hook
    ((dired-mode-hook . g-dired-disable-line-wrapping)
     (dired-after-readin-hook . g-dired-postprocess-ls-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hc-dired)

;;; hc-dired.el ends here
