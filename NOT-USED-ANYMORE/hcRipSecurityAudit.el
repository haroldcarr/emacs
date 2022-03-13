;;;;
;;;; Created       : 1999 Oct 25 (Mon) 10:39:05 by Harold Carr.
;;;; Last Modified : 1999 Dec 12 (Sun) 17:33:44 by Harold Carr.
;;;;

(require 'hcChangeWords)

(defvar *ripSrc* "")
(setq *ripSrc* (concat (ripHomeColon) "/src/share/classes"))
(defun ripSrc () *ripSrc*)

(defun hcRipSecurityAudit (files lst)
  (mapc #'(lambda (file) (hcRipSecurityAuditAux file lst))
	(mapcar #'(lambda (file) (concat (ripSrc) file))
		files)))

(defun hcRipSecurityAuditAux (file lst)
  (hcChangeWords lst file "/tmp" t))

(defvar *hcRSAPrivileged*
  '(
    ("Privileged"            . "Privileged")

    ("ClassLoader"           . "ClassLoader")

    ("getParent"             . "getParent")
    ("getSystemClassLoader"  . "getSystemClassLoader")

    ("getContextClassLoader" . "getContextClassLoader")

    ("forName"               . "forName")

    ("getClassLoader"        . "getClassLoader")

    ("newInstance"           . "newInstance")
    ("getClasses"            . "getClasses")
    ("getField"              . "getField")
    ("getMethod"             . "getMethod")
    ("getConstructor"        . "getConstructor")
    ("getDeclaredClasses"    . "getDeclaredClasses")
    ("getDeclaredField"      . "getDeclaredField")
    ("getDeclaredMethod"     . "getDeclaredMethod")
    ("getDeclaredConstructor" . "getDeclaredConstructor")
    ))

(defvar *hcRSAPublicStatic*
  '(
    ("public static"         . "public static")
    ))

(defvar *hcRSAStatic*
  '(
    ("static"         . "static")
    ))

(defvar *hcRSAException*
  '(
    ("AccessControlException" . "AccessControlException")
    ("SecurityException" . "SecurityException")
    ))

(defvar *hcRSAIterate*
  '(
    ("class" . "class")
    ("interface" . "interface")
    ))

(comment
(load-library "hcRipSecurityAudit")
(hcRipSecurityAudit *Subcontract* *hcRSAPrivileged*)
(hcRipSecurityAudit *Core*        *hcRSAPrivileged*)
(hcRipSecurityAudit *Transport*   *hcRSAPrivileged*)

(hcRipSecurityAudit *Subcontract* *hcRSAPublicStatic*)
(hcRipSecurityAudit *Core*        *hcRSAPublicStatic*)
(hcRipSecurityAudit *Transport*   *hcRSAPublicStatic*)

(hcRipSecurityAudit *Subcontract* *hcRSAStatic*)
(hcRipSecurityAudit *Core*        *hcRSAStatic*)
(hcRipSecurityAudit *Transport*   *hcRSAStatic*)

(hcRipSecurityAudit *Subcontract* *hcRSAException*)
(hcRipSecurityAudit *Core*        *hcRSAException*)
(hcRipSecurityAudit *Transport*   *hcRSAException*)

(hcRipSecurityAudit *Subcontract* *hcRSAIterate*)
(hcRipSecurityAudit *Core*        *hcRSAIterate*)
(hcRipSecurityAudit *Transport*   *hcRSAIterate*)
)

;;; End of file.

