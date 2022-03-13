;;;;
;;;; Created       : 1998 Apr 15 (Wed) 12:03:01 by Harold Carr.
;;;; Last Modified : 1999 Dec 12 (Sun) 11:39:36 by Harold Carr.
;;;;

;;;
;;; ToDo: do not communicate through globals
;;;

(require 'hcListFilesFromMss)
(require 'hcChangeWords)

(defun hcCheckMss (words-file mss-location base-dir)
  "Check for words that need to be changed.
 Given:
  * words-file used by change-words.
  * location of VSS MSS file used by list-files-from-mss.
  * a base directory to prepend to result o list-files-from-mss
    before doing change-words."
  (save-selected-window
    (find-file-read-only words-file)
    (eval-buffer nil)
    (kill-buffer (current-buffer))
    (let ((files (mapcar '(lambda (x)
			    (let ((from-location (car x))
				  (to-location   (cdr x)))
			      (cons (hc-dir-make-full-path base-dir
							   from-location)
				    to-location)))
			 (hcListFilesFromMss mss-location))))
      
      (mapcar '(lambda (x)
		 (let ((from-location (car x))
		       (to-location   (cdr x)))
		   (hcChangeWords *basis* from-location "c:/temp"
				    t to-location)))
	      files))))

(provide 'hcCheckMss)

;;; End of file.
