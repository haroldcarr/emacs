;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Check my request list with Basis change list.
;;;;

(defun display-differences (my-list basis-list)
  (let* ((tmp (diff-alists my-list basis-list))
	 (ad  (car tmp))
	 (basis (car (cdr tmp))))
    (princ "==================================================\n")
    (princ "Autodesk requests not done:\n\n")
    (display-differences-aux ad)
    (princ "==================================================\n")
    (princ "Basis changes not requested:\n\n")
    (display-differences-aux basis)))

(defun display-differences-aux (lis)
  (mapc '(lambda (x) (princ (format "%S" (car lis))) (terpri)) lis))

(defun diff-alists (alist1 alist2)
  (let ((one-not-in-two (diff-alists-aux alist1 alist2))
	(two-not-in-one (diff-alists-aux alist2 alist1)))
    (list one-not-in-two two-not-in-one)))

(defun diff-alists-aux (alist1 alist2)
  (let ((result '()))
    (while alist1
      (if (not (string-ci-assoc-p (car alist1) alist2))
	  (setq result (cons (car alist1) result)))
      (setq alist1 (cdr alist1)))
    (reverse result)))

;;;
;;; Both the CAR and the CDR have to match.
;;; Case is ignored.
;;;

(defun string-ci-assoc-p (el alist)
  (let ((result nil))
    (while alist
      (cond ((and (string-equal (downcase (car el)) 
				(downcase (car (car alist))))
		  (string-equal (downcase (cdr el))
				(downcase (cdr (car alist)))))
	     (setq result t)
	     (setq alist nil))
	    (t
	     (setq alist (cdr alist)))))
    result))

