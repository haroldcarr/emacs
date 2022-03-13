;;;;
;;;; Emacs LISP program to find and change all uses of given words
;;;; in all files and subdirectories of a given directory.
;;;;
;;;; Created       : 1998 Apr 14 (Tue) 16:45:55 by Harold Carr.
;;;; Last Modified : 1999 Dec 11 (Sat) 20:24:10 by Harold Carr.
;;;;

;;;
;;; ToDo:
;;;
;;; * Do not communicate with eval buffers via globals.
;;;

(defun hcListFilesFromMss (filename)
  "Parses a VSS MSS file to extract the \"from\" files."
  (if (file-directory-p filename)
      (error "Not a file: %S" filename)
      (hcListFilesFromMssAux filename)))

(defun hcListFilesFromMssAux (filename)
  "Helper for hcListFilesFromMss."
  (save-selected-window
    (save-excursion
      (save-restriction
	(save-match-data
	  (find-file-read-only filename)
	  (let ((result-buffer (get-buffer-create "TT"))
		(mss-buffer (current-buffer)))
	    (switch-to-buffer result-buffer)
	    (insert "(setq *extraction-results* (quote (\n")
	    (switch-to-buffer mss-buffer)
	    (widen)
	    (goto-char (point-min))
	    (search-forward "#Files" nil t)
	    (while (search-forward ";" nil t 5)
	      (let ((beginning (point)))
		(search-forward ";" nil t)
		(copy-region-as-kill beginning (- (point) 1))
		(switch-to-buffer result-buffer)
		(insert "(\n")
		(hcListFilesFromMss-grabMatch)
		(insert ".\n")
		(switch-to-buffer mss-buffer)
		(setq beginning (point))
		(search-forward ";" nil t)
		(copy-region-as-kill beginning (- (point) 1))
		(switch-to-buffer result-buffer)
		(hcListFilesFromMss-grabMatch)
		(insert ")\n")
		(switch-to-buffer mss-buffer)
		(end-of-line)))
	    (switch-to-buffer result-buffer)
	    (insert ")))\n")
	    (eval-buffer nil)
	    (kill-buffer result-buffer)
	    (kill-buffer mss-buffer)
	    *extraction-results*))))))

(defun hcListFilesFromMss-grabMatch ()
  (yank)
  (beginning-of-line)
  (replace-string "\\" "/")
  (beginning-of-line)
  (insert "\"")
  (end-of-line)
  (insert "\"\n"))


(provide 'hcListFilesFromMss)

;;; End of file.
