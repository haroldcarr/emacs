(if (locate-file "agda-mode" exec-path exec-suffixes 1)
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
  (message "agda-mode not on path"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From appendix of Verified Functional Programming in Adga

(comment
(eval-after-load "quail/latin-ltx"
  '(mapc
    (lambda (pair) (quail-defrule (car pair) (cadr pair) "TeX"))
    '( ("\\bb"   "B")    ("\\bl" "L") ("\\bs" "S")
       ("\\bt"   "T")    ("\\bv" "V") ("\\cv" "O ")
       ("\\comp" "  ") ("\\m" "ÞÑ") ("\\om" "ω"))))
)

(comment
; Change Control-c Control-, and Control-c Control-.
; in Agda mode so they show the normalized rather
; than the "simplified" goals

(defun agda2-normalized-goal-and-context ()
  (interactive)
  (agda2-goal-and-context '(3)))

(defun agda2-normalized-goal-and-context-and-inferred ()
  (interactive)
  (agda2-goal-and-context-and-inferred '(3)))

(eval-after-load "agda2-mode"
  '(progn
     (define-key agda2-mode-map (kbd "C-c C-,")
       'agda2-normalized-goal-and-context)
     (define-key agda2-mode-map (kbd "C-c C-.")
       'agda2-normalized-goal-and-context-and-inferred)))
)

; This sets the Agda include path.
; Change YOUR-PATH to the path on your computer
; to the Iowa Agda Library directory. Use forward
; slashes to separate subdirectories.
;(custom-set-variables
;'(agda2-include-dirs
;(quote ("." "YOUR-PATH/ial")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'hc-agda)

