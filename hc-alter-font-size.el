;; http://emacsninja.com/posts/making-emacs-more-presentable.html

(defun my-alter-frame-font-size (fn)
  (let* ((current-font-name (frame-parameter nil 'font))
         (decomposed-font-name (x-decompose-font-name current-font-name))
         (font-size (string-to-int (aref decomposed-font-name 5))))
    (aset decomposed-font-name 5 (int-to-string (funcall fn font-size)))
    (set-frame-font (x-compose-font-name decomposed-font-name))))

(defun my-inc-frame-font-size ()
  (interactive)
  (my-alter-frame-font-size '1+))

(defun my-dec-frame-font-size ()
  (interactive)
  (my-alter-frame-font-size '1-))

;; (global-set-key (kbd "C-+") 'my-inc-frame-font-size)
;; (global-set-key (kbd "C-=") 'my-inc-frame-font-size)
;; (global-set-key (kbd "C--") 'my-dec-frame-font-size)
