;;; hc-font-size-frame-size --- font

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;; ------------------------------------------------------------------------------
;; ------------------------------------------------------------------------------
;; * frame/font size

(hcSection "frame/font size")

;; C-U C-X : shows current font

;; C-x C-- : decrease font size
;; C-x C-+ : increase font size
;; C-x C-0 : reset to default size
;; these run text-scale-adjust

(defun hc-h (n)    "N."   (interactive) (set-frame-height (selected-frame) n))
(defun hc-w (n)    "N."   (interactive) (set-frame-width (selected-frame) n))
(defun hc-hw (x y) "X Y." (interactive) (hc-h x) (hc-w y))
(defun hc-hwd ()   "."    (interactive) (hc-h 27) (hc-w 101))

(defun hcFonts (default-height variable-pitch-height)
  "DEFAULT-HEIGHT VARIABLE-PITCH-HEIGHT."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Monaco"
                      :height variable-pitch-height
                      :weight 'regular))

(defun hcDoFonts () "."
  (interactive)
  (when window-system
    (cond ((> (x-display-pixel-width) 1800)
           (hcFonts 200 300))
          (t (hcFonts 175 200)))))

(defun hc-font-size (n) "N."
  (interactive "nSize: ")
  (hcFonts (* n 10) (* n 15)))

;; ------------------------------------------------------------------------------
;; https://github.com/zonuexe/emacs-presentation-mode

;; this one changes font size but keeps frame size the same

(use-package presentation)

;; ------------------------------------------------------------------------------
;; http://emacsninja.com/posts/making-emacs-more-presentable.html

;; this one shrinks/enlarges frame

(defun hc-alter-frame-font-size (fn)
  (let* ((current-font-name (frame-parameter nil 'font))
         (decomposed-font-name (x-decompose-font-name current-font-name))
         (font-size (string-to-number (aref decomposed-font-name 5))))
    (aset decomposed-font-name 5 (int-to-string (funcall fn font-size)))
    (set-frame-font (x-compose-font-name decomposed-font-name))))

(defun hc-inc-frame-font-size ()
  (interactive)
  (hc-alter-frame-font-size '1+))

(defun hc-dec-frame-font-size ()
  (interactive)
  (hc-alter-frame-font-size '1-))

;; (global-set-key (kbd "C-c C-+") 'hc-inc-frame-font-size)
;; (global-set-key (kbd "C-c C-=") 'hc-inc-frame-font-size)
;; (global-set-key (kbd "C-c C--") 'hc-dec-frame-font-size)

(provide 'hc-font-size-frame-size)

;;; hc-font-size-frame-size.el ends here
