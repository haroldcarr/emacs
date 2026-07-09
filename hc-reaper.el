;;; hc-reaper --- Summary           -*- lexical-binding: t; -*-

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2026 Jul 02 (Thu) 11:14:02 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(defun hc-launch-reaper-like-finder ()
  "Launch REAPER on macOS."
  (interactive)
  (start-process "REAPER" nil "open" "-a" "REAPER"))


(defun hc-launch-reaper-with-current-env ()
  "Launch REAPER on macOS."
  (interactive)
  (start-process "REAPER" nil "/Applications/REAPER.app/Contents/MacOS/REAPER"))

(provide 'hc-reaper)

;;; hc-reaper.el ends here

