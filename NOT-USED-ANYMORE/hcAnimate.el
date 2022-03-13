;;;;
;;;; Created       : 1999 Jun 18 (Fri) 18:38:05 by Harold Carr.
;;;; Last Modified : 1999 Jun 19 (Sat) 11:34:00 by Harold Carr.
;;;;

(defun hcAnimateDir ()
  (interactive)
  (let ((stream (find-file-noselect ".animate.html")))
    (hcAnimate "." stream)
    (with-current-buffer stream
      (save-buffer stream))
    (kill-buffer stream)
    t))

(defun hcAnimate (listOrDirectory &optional stream)
  (if (null stream) (setq stream (current-buffer)))
  (let* ((stringp (stringp listOrDirectory))
	 (len (if stringp
		  (- (length (directory-files listOrDirectory)) 2)
		(length listOrDirectory)))
	 (files (if stringp
		    (directory-files listOrDirectory)
		  listOrDirectory))
	 (i 0))
    (hcP `("<html><body>"
	   ,(format "<img src=%S name=animation>" (car files))
	   "<script>"
	   ,(format "var images = new Array(%s);" len))
	 stream)
    (mapc
     #'(lambda (name)
	 (let ((name (file-name-nondirectory name)))
	   (cond ((let ((ext (file-name-extension name)))
		    (and ext (equal (downcase ext) "jpg")))
		  (hcP `(,(format "images[%S] = new Image();" i)
			 ,(format "images[%S].src = %S;" i name))
		       stream)
		  (setq i (+ i 1))))))
     files)
    (hcP `(,(format "var numberOfFiles = %S;" i)
	   "function step(amount)"
	   "{"
	   "    frame = (frame + amount)%numberOfFiles;"
	   "    if (frame < 0) frame = -frame;"
	   "    document.animation.src = images[frame].src;"
	   "}"
	   "function animate()"
	   "{"
	   "    step(1);"
	   "    timeout_id = setTimeout(\"animate()\", 1000);"
	   "}"
	   "var frame = -1;"
	   "var timeout_id = null;"
	   "</script>"
	   "<form>"
	   "<input type=button value=\"Start\""
	   "       onClick=\"if (timeout_id == null) animate()\">"
	   "<input type=button value=\"Stop\""
	   "       onClick=\"if (timeout_id) clearTimeout(timeout_id); timeout_id=null;\">"
	   "<br>"
	   "<input type=button value=\"Back 25\""
	   "       onClick=\"step(-25);\">"
	   "<input type=button value=\"Forward 25\""
	   "       onClick=\"step(25);\">"
	   "<br>"
	   "<input type=button value=\"Step Back\""
	   "       onClick=\"step(-1);\">"
	   "<input type=button value=\"Step Forward\""
	   "       onClick=\"step(1);\">"
	   "</form>"
	   "</body>"
	   "</html>")
	 stream)))

(defun hcP (lines &optional stream)
  (if (null stream) (setq stream (current-buffer)))
  (mapc #'(lambda (line) (princ line stream) (terpri stream))
	lines))

(comment
(load-library "hcAnimate")
(hcAnimate '("a.jpg" "b.JPG"))
)

(provide 'hcAnimate)

;;; End of file.

















