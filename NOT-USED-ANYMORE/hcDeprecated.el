;;;;
;;;; Created       : 2012 Nov 09 (Fri) 09:09:09 by carr.
;;;; Last Modified : 2012 Nov 09 (Fri) 11:57:46 by carr.
;;;;

(defun hcPackageBase () "com.oracle.webservices.")

(defun hcNewPackage ()
  (concat (hcPackageBase) "impl.disi.tube."))

(defun hcClassname ()
  (let ((bn (buffer-name)))
    (substring bn 0 (- (length bn) (length ".java")))))

(defun hcWhere ()
  (concat (hcNewPackage) (hcClassname)))

(defun hcBeginJavaDoc     ()      "/**\n")
(defun hcAtDeprecated     (where) (concat " * @deprecated - use " where "\n"))
(defun hcAtSee            (where) (concat " * @see "              where "\n"))
(defun hcEmptyJavaDocLine ()      " *\n")
(defun hcEndJavaDoc       ()      " */\n")

(defun hcAtDeprecatedAtSee (where)
  (concat
   (hcAtDeprecated where)
   (hcAtSee        where)))

(defun hcDeprecated (&optional n)
  (interactive "p")
  (insert
   (concat
    (if (= n 4) (hcBeginJavaDoc))
    (hcAtDeprecatedAtSee (hcWhere))
    (if (not (= n 4)) (hcEmptyJavaDocLine))
    (if (= n 4) (hcEndJavaDoc)))))

(global-set-key "\C-]" 'hcDeprecated)
;; RESET
;(global-set-key "\C-]" 'abort-recursive-edit)

