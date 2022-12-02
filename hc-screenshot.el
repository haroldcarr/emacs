;;; hc-screenshot.el --- idris          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------------------------------------------------------
;; https://github.com/tarsius/frameshot

(use-package frameshot
  :ensure nil)

;; ------------------------------------------------------------------------------
;; http://mbork.pl/2022-08-27_Screenshots_from_Emacs

(defvar x-frameshot-directory "~/Pictures/Screenshots/"
  "Default directory for frame shots.")

(defvar x-frameshot-format 'png
  "Default frame shot format.  See doc for `x-export-frames` for supported formats.")

(defun x-frameshot ()
  "Save Emacs frame as frame shot.
Directory is determined by variable `x-frameshot-directory' and if
not defined, it will be saved in the `$HOME' directory."
  (interactive)
  (let* ((image          (x-export-frames nil (or x-frameshot-format 'png)))
	 (base-directory (or x-frameshot-directory (getenv "HOME")))
	 (directory      (concat (file-name-as-directory base-directory)
			         (format-time-string "%Y/%m/%Y-%m-%d/")))
	 (file           (concat directory
                                 (format-time-string "Screenshot-%Y-%m-%d-%T.")
		                 (symbol-name x-frameshot-format))))
    (make-directory directory t)
    (with-temp-file file
      (insert image))
    (dired directory)
    (revert-buffer)
    (dired-goto-file (expand-file-name file))
    (message "Frame shot saved as `%s'" file)))

(provide 'hc-screenshot)

;;; hc-screenshot.el ends here
