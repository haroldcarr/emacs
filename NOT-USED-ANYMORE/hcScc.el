;;;;
;;;; Created       : 1999 May 21 (Fri) 11:23:26 by Harold Carr.
;;;; Last Modified : 1999 Jul 13 (Tue) 14:09:24 by Harold Carr.
;;;;

;;;
;;; hc-check-out
;;;
;;;  ;;(vc-next-action-on-file full-path-and-name nil)

(defun hc-check-out (really-do-it full-path-and-name buffer)
  (hcMessage (format "Checking out: %s" full-path-and-name))
  (if really-do-it
      (vc-checkout full-path-and-name t)))

;;;
;;; hc-check-in
;;;

;; To much overhead:
;; (vc-next-action-on-file (buffer-file-name ..)  verbose comment)

(defun hc-check-in (really-do-it full-path-and-name buffer check-in-comment)
  (hcMessage (format "Checking in or reverting: %s" buffer))
  (cond ((buffer-modified-p buffer)
	 (hcMessage (format "Checking in: %s" buffer))
	   (if really-do-it
	       (save-excursion
		 (switch-to-buffer buffer)
		 (save-buffer)
		 (vc-checkin full-path-and-name nil check-in-comment))))
	  (t
	   (hcMessage (format "Reverting: %s" buffer))
	   (if really-do-it
	       (let ((curDir default-directory))
		 ;; Must do revert in real dir or it leaves
		 ;; the reverted file in the working directory
		 ;; of this program and leaves the old file writable
		 ;; (it does do the unget right).
		 (cd (file-name-directory full-path-and-name))
		 (vc-backend-revert full-path-and-name)
		 ;; ***** Something is crashing XEmacs, maybe this?
		 ;; Hack when it was in wrong dir
		 ;;(apply 'call-process "chmod" nil nil nil (list "-w" full-path-and-name))
		 (cd curDir))))))

(provide 'hcScc)

;;; End of file.
