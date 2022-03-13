;;;;
;;;; Created       : 1999 May 15 (Sat) 09:54:15 by Harold Carr.
;;;; Last Modified : 2002 May 17 (Fri) 07:54:00 by Harold Carr.
;;;;

(require 'hcSh)
(require 'hcJde-2.1.5)
(require 'hcJdeRipCommon)

;;;;
;;;; RIP Ease-of-use for JDE.
;;;;

(hcShDefCmd ripOrbdArgs           ())
(hcShDefCmd ripOrbdMainClass      ())
(hcShDefCmd ripTnameservArgs      ())
(hcShDefCmd ripTnameservMainClass ())
(hcShDefCmd ripPlatform           ())
(hcShDefCmd ripClientArgs         ())
(hcShDefCmd ripServerArgs         ())

;;;
;;; Setup paths.
;;;

(defun hcJdeRip-2.1.5-setup (javaHome ripHome sourcePaths vmArgs classpath)
  (let ((platform  (ripPlatform))
	(ps        jde-classpath-separator))
    (hcJde-2.1.5-setup
     javaHome
     (funcall vmArgs      javaHome ripHome platform ps)
     (funcall classpath   javaHome ripHome platform ps)
     (funcall sourcePaths javaHome ripHome platform ps))
    ;; Prepare JVM to find ioser.
    (cond ((equal platform "win32")
	   (setenv "PATH" 
		   (concat ripHome "/build/" platform "/bin" ps
			   (getenv "PATH"))))
	  (t
	   (let ((LD (concat ripHome "/build/" platform "/lib/sparc")))
	     (setenv "LD_LIBRARY_PATH"
		     (concat LD ps (getenv "LD_LIBRARY_PATH"))))))))
	     

;;;
;;; Functions to stepping various starting points.
;;;

(defun hcJdeRip-2.1.5-vmArgsForTest (javaHome ripHome platform ps)
  (concat " -Dhttp.server.port=9090"
	  " -Djava.rmi.server.codebase=http://localhost:9090/"
	  " -Dhttp.server.root.directory=" 
	              ripHome "/values/build/" platform "/classes/"
	  " -Djava.security.policy=" ripHome "/test/src/share/classes/test.policy"))

(defun hcJdeRip-2.1.5-runTest (javaHome ripHome test-list)
  (hcJdeRip-2.1.5-setup javaHome 
		  ripHome 
		  'hcJdeRipCommon-sourcePathlist
		  'hcJdeRip-2.1.5-vmArgsForTest
		  'hcJdeRipCommon-classpath)
  (hcJde-2.1.5-runOrDebug
   'debug
   ;; Directory to start in.
   (concat ripHome "/test/build/" (ripPlatform))
   ;; Main
   "test.Test"
   ;; Args to main
   (concat "-file " ripHome "/test/src/share/classes/" test-list
	   " -verbose"
	   " -output " ripHome "/gen")))

(defun hcJdeRip-2.1.5-runOrbd (javaHome ripHome)
  (hcJdeRip-2.1.5-setup javaHome 
		  ripHome 
		  'hcJdeRipCommon-sourcePathlist
		  'hcJdeRipCommon-vmArgs
		  'hcJdeRipCommon-classpath)
  (hcJde-2.1.5-runOrDebug
   'debug
   ;; Directory to start in.
   (concat (expand-file-name "~"))
   ;; Main
   (ripOrbdMainClass)
   ;; Args to main
   (ripOrbdArgs)))

(defun hcJdeRip-2.1.5-runTnameserv (javaHome ripHome)
  (hcJdeRip-2.1.5-setup javaHome 
		  ripHome 
		  'hcJdeRipCommon-sourcePathlist
		  'hcJdeRipCommon-vmArgs
		  'hcJdeRipCommon-classpath)
  (hcJde-2.1.5-runOrDebug
   'debug
   ;; Directory to start in.
   (concat (expand-file-name "~"))
   ;; Main
   (ripTnameservMainClass)
   ;; Args to main
   (ripTnameservArgs)))

(defun AllOrRipTests (test-list)
  (hcJdeRip-2.1.5-runTest (BOOTDIR) (ripHomeColon) test-list))

(defun AllTests ()
  (interactive)
  (AllOrRipTests "test/AllTests.txt"))

(defun RipTests ()
  (interactive)
  (AllOrRipTests "rip/RipTests.txt"))

(defun orbd ()
  (interactive)
  (hcJdeRip-2.1.5-runOrbd (BOOTDIR) (ripHomeColon)))

;; To run this you must remove InitialHost from bin/ripVmArgs
(defun tnameserv ()
  (interactive)
  (hcJdeRip-2.1.5-runTnameserv (BOOTDIR) (ripHomeColon)))

;;;;
;;;; General purpose startup rip shell
;;;;

(defun hcJdeRip-2.1.5-shell (main argsToMain)
  (hcJdeRip-2.1.5-setup (BOOTDIR)
		  (ripHomeColon)
		  'hcJdeRipCommon-sourcePathlist
		  'hcJdeRipCommon-vmArgs
		  'hcJdeRipCommon-classpath)
  (hcJde-2.1.5-runOrDebug
   'debug
   ;; Directory to start in.
   (concat (expand-file-name "."))
   main
   argsToMain))

;; Useful to create startup rip shells given a class path/name.
;; The main class becomes the name of the function.
;; The full path/name is entered with no args.

(defmacro hcJdeRip-2.1.5-defShell (name)
  (let* ((className (symbol-name name))
	 (functionName (intern
			(car (nreverse (split-string className "\\."))))))
    `(defun ,functionName ()
       (interactive)
       (hcJdeRip-2.1.5-shell ,className " "))))

(provide 'hcJdeRip-2.1.5)

;;;;
;;;; Test and usage.
;;;;

(comment
(load-library "hcJdeRip-2.1.5.el")
(RipTests)
(AllTests)
(orbd)
(ex)
)

;;; End of file.
