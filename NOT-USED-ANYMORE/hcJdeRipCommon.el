;;;;
;;;; Created       : 2002 Mar 15 (Fri) 14:31:43 by Harold Carr.
;;;; Last Modified : 2003 Jun 03 (Tue) 11:56:31 by Harold Carr.
;;;;

(require 'hcSh)

;;; Stuff that used to live in hcJdeRip.
;;; Now used by all versions.

(hcShDefCmd ripClasspath     (javaHome rmiIiopHome))
(hcShDefCmd ripVmArgs        ())

(defun hcJdeRipCommon-sourcePathlist (javaHome ripHome platform ps)
  (let* ((processor (cond ((equal platform "solaris") "sparc")
			  (t (error "HC FIX PLATFORM"))))
	 (path
	  (concat (hcJdeRipCommon-sourcePathlistExtras ps)
		  ;; Put java source first to find org.omg.* first
		  ;; (like class loader).
		  javaHome "/src/"                        ps
                  ripHome "/build/" processor "/gensrc/"   ps
		  ripHome "/src/share/classes/"           ps
		  ripHome "/test/src/share/classes/"      ps
		  ripHome "/test12/src/share/classes/"    ps
		  ripHome "/values/src/share/classes/"    ps
		  ripHome "/values2/src/share/classes/"   ps
		  ripHome "/test/build/" processor "/classes"
		  )))
    (message (format "DEBUG: %s" path))
    path))

(defun hcJdeRipCommon-classpath (javaHome ripHome platform ps)
  (concat
   (hcJdeRipCommon-classpathExtras ps)
   (ripClasspath javaHome ripHome)))

(defun hcJdeRipCommon-vmArgs (javaHome ripHome platform ps)
  (concat
   (ripVmArgs)
   " "
   ;; This should come last so they override any args already
   ;; specified in ripVmArgs (e.g., the orb class).
   (hcJdeRipCommon-vmArgsExtras)))

;;; Hooks so you can add to the above.

(defun hcJdeRipCommon-sourcePathlistExtras (ps)  "")
(defun hcJdeRipCommon-vmArgsExtras         ()    "")
(defun hcJdeRipCommon-classpathExtras      (ps)  "")

(provide 'hcJdeRipCommon)

;;; End of file.