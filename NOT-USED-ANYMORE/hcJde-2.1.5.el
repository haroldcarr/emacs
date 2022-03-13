;;;;
;;;; Created       : 1999 May 15 (Sat) 09:54:15 by Harold Carr.
;;;; Last Modified : 2002 Mar 15 (Fri) 21:50:30 by Harold Carr.
;;;;

(require 'hcSh)

(defmacro comment (&rest x) nil)

(hcShDefCmdMemo ripPlatform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find JDE and load it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hcJde-2.1.5-path (concat (hcEmacsExtPkg) "/jde/jde-2.1.5"))

(defvar hcJde-2.1.5-foundP nil "Does JDE live where it is supposed to?")

(defun hcJde-2.1.5-modeHook () 
  "This function gets run whenever you visit a *.java file."
  ;; I do not want colors.
  (setq jde-use-font-lock nil)
  ;; Use this rather than "prj.el" default.
  (setq jde-project-file-name "jdePrj.el"))

(defun hcJde-2.1.5-findAndLoadJde ()
  (cond ((file-directory-p hcJde-2.1.5-path)
	 (hcPushOn hcJde-2.1.5-path load-path)
	 (require 'jde)
	 (setq hcJde-2.1.5-foundP t)
	 (hcJde-2.1.5-modeHook)
	 (add-hook 'jde-modeHook 'hcJde-2.1.5-modeHook))))

(hcJde-2.1.5-findAndLoadJde)

;;;
;;; Call this to run or debug a specific application.
;;;

(defun hcJde-2.1.5-runOrDebug (runOrDebug wd main appArgs)
  (setq jde-run-working-directory wd)
  (jde-run-set-app main)
  (cond ((eq runOrDebug 'run)
	 (jde-run-set-app-args appArgs)
	 (jde-run))
	((eq runOrDebug 'debug)
	 (jde-db-set-app-args appArgs)
	 (jde-db))
	(t
	 (error "hcJde-2.1.5-runOrDebug: unknown: %a" runOrDebug))))

;;;
;;; Call this to set up paths.
;;;

(defun hcJde-2.1.5-setup (javaHome vmArgs classpath sourcePaths)
    ;; Set source package roots so emacs can step into source.
    (jde-db-set-source-paths sourcePaths)

    ;; Specify the VM.
    (funcall (if (equal (ripPlatform) "win32")
		 'jde-run-set-vm-w 
	       'jde-run-set-vm)
	     (concat javaHome "/bin/java"))

    ;; Specify the debugger.
    (jde-db-set-debugger  (concat javaHome "/bin/oldjdb") t)
    ;;(jde-db-set-debugger  (concat javaHome "/bin/jdb") t)

    ;; Specify args (other than classpath) to VM and debugger.
    (jde-run-set-args vmArgs)
    (jde-db-set-args  vmArgs)

    ;; Specify classpath.
    (jde-set-global-classpath classpath))

;;;;
;;;; JDE notes.
;;;;

(comment
   "Useful jde commands and variables."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq jde-jdk-doc-url
      "http://www.javasoft.com/products/jdk/1.2/docs/index.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile, run, debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(jde-set-global-classpath "c:/foo; c:/temp;c:/tmp;d:/tmp;d:/bar")
(jde-set-global-classpath "c:\\foo;c:/temp;c:/tmp;d:/tmp;d:/bar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run, debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The directory in which to start VM
(setq jde-run-working-directory "c:/temp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jde-run-java-vm - the interpreter - non windows
(jde-run-set-vm "")

;; jde-run-java-vm-w - the interpreter - windows
(jde-run-set-vm-w "")

;; jde-run-option-vm-args - args to VM
(jde-run-set-args "")

;; jde-run-application-class - the class to execute main
(jde-run-set-app "")

;; jde-run-options-application-args - args to application
(jde-run-set-app-args "")

;; do it
(jde-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jde-db-debugger - the debugger (executable or class)
(jde-db-set-debugger "c:/temp/javadbg" t)

;; jde-db-source-directories - root of package sources
(jde-db-set-source-paths "c:/foo;c:/temp;c:/tmp;d:/tmp;d:/bar")

;; jde-db-option-vm-args - args to VM during debugging
(jde-db-set-args "")

;; jde-db-option-application-args
(jde-db-set-app-args "")

;; do it
(jde-db)
) ;; end of comment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff to look at skij.

(defun hcStepSkij ()
  (interactive)
  (hcJde-2.1.5-setup 
   ;; java home
   (concat (usrLocalHcDir) "/java/jdk1.2.1")
   ;; vm args
   ""
   ;; classpath
   (concat 
    "."
    (hcPathSep) 
    (usrLocalHcDir) "/java/skij/skij-1-7-3/skij.jar"
    (hcPathSep)
    (usrLocalHcDir) "/java/jdk1.2.1/jre/lib/rt.jar"
    (hcPathSep)
    (expand-file-name "~/.sync/.esync/hc/java/classes"))
   ;; sourcePaths
   (concat
    (expand-file-name "~/.sync/.esync/hc/java")
    (hcPathSep)
    (usrLocalHcDir) "/java/skij/skij-1-7-3/skijsrc/"))
  (hcJde-2.1.5-runOrDebug
   'debug
   "."
   "com.ibm.jikes.skij.Scheme"
   ""))

(comment
(load-library "hcJde-2.1.5")
(hcStepSkij)
)

(provide 'hcJde-2.1.5)

;;; End of file.

