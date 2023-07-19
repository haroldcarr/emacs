;;; hc-alter-font-size --- font

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

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

(provide 'hc-alter-font-size)

;;; hc-alter-font-size.el ends here
