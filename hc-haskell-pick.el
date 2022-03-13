;;; hc-haskell-pick --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2022 Mar 13 (Sun) 13:24:37 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

(defvar hc-haskell-pick)

(defun hc-pick-haskell-support ()
  "Prompt pick from a list."
  (interactive)
  (let ((choices '("hls" "none")))
    (message "%s" (ido-completing-read "which haskell?: " choices))))

(let ((pick (hc-pick-haskell-support)))
  (cond ((equal pick "hls")
         (message "Using HLS")
         (setq hc-haskell-pick 'hls)
         (use-package hc-haskell-hls))
        ((equal pick "none")
         (message "NO HASKELL SUPPORT")
         (setq hc-haskell-pick 'none))
        (t
         (message "NO HASKELL MATCH %s" pick))))

;;;;;;;;;;;;;

(provide 'hc-haskell-pick)

;;; hc-haskell-pick.el ends here

