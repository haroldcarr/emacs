;;;;
;;;; Created       : 1999 May 15 (Sat) 09:54:15 by Harold Carr.
;;;; Last Modified : 2003 Dec 18 (Thu) 08:23:27 by Harold Carr.
;;;;

(require 'hcSh)
(require 'hcJdeRipCommon)

(defmacro comment (&rest x) nil)

(hcShDefCmdMemo ripPlatform)
(hcShDefCmdMemo hcPathSep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find JDE and load it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hcJde-path (concat (hcEmacsExtPkg) "/jde/jde-2.2.9beta10/lisp"))

(defvar hcJde-foundP nil "Does JDE live where it is supposed to?")

(defun hcJde-pushPath ()
  (add-to-list 'load-path 
	       hcJde-path)
  (add-to-list 'load-path 
	       (concat (hcEmacsExtPkg) "/semantic/semantic-1.4"))
  (add-to-list 'load-path 
	       (concat (hcEmacsExtPkg) "/speedbar/speedbar-0.14beta4"))
  (add-to-list 'load-path 
	       (concat (hcEmacsExtPkg) "/elib/elib-1.0"))
  (add-to-list 'load-path
	       (concat (hcEmacsExtPkg) "/eieio/eieio-0.17beta4"))
  )

(defun hcJde-modeHook () 
  "This function gets run whenever you visit a *.java file."
  (setq jde-use-font-lock nil)  ;; I do not want colors.
  (custom-set-variables
   '(jde-project-context-switching-enabled-p nil) ; Manually load prj.el.
   '(jde-expand-classpath-p nil) ; Don't look in "*/lib" looking for jars.
   ))

(defun hcJde-findAndLoadJde ()
  (cond ((file-directory-p hcJde-path)
	 (hcJde-pushPath)
	 (require 'jde)
	 (setq hcJde-foundP t)
	 ;(hcJde-defineKeys)
	 (hcJde-modeHook)
	 (add-hook 'jde-modeHook 'hcJde-modeHook))
	(t
	 (message "hcJde-findAndLoadJde failed."))))

;; Note: "duplicated" in jde-bug - works with both...
(defun hcJde-defineKeys ()
  (let ((bindings
	 '(("[?\C-x ?\C-a ?\C-a ?\C-e]" . jde-bug-evaluate-expression)
	   ("[?\C-x ?\C-a ?\C-a ?\C-l]" . jde-bug-attach-local-host)
	   ("[?\C-x ?\C-a ?\C-a ?\C-v]" . jde-bug-display-variable)
	   ("[?\C-x ?\C-a ?\C-a ?\C-a]" . jde-bug-display-array)
	   ("[?\C-x ?\C-a ?\C-a ?\C-o]" . jde-bug-display-object)
	   ("[?\C-x ?\C-a ?\C-a ?\C-s]" . jde-bug-display-string))
	 ))
    (mapc (lambda (binding)
	    (let ((key (car (read-from-string (car binding))))
		  (fcn (cdr binding)))
	      (message "setting key to nil")
	      (define-key jde-jdb-mode-map key nil)
	      (message "setting key to fcn")
	      (define-key jde-jdb-mode-map key fcn)))
	  bindings)))

(hcJde-findAndLoadJde)

;;;
;;; Ease-of-use entry point into debugging.
;;;

(comment
   `(,(if (equal (ripPlatform) "win32")
	  'jde-run-java-vm
	'jde-run-java-vm-w)
     ,(concat javaHome "/bin/java"))
)

(defvar *hcjdb* 'JDEbug)
(defun hcjdb (x) 
  (if (not (member x '(JDEbug jdb)))
      (error "UNKNOWN"))
  (setq *hcjdb* x))

(defun hcJde-debug (wd sourcePath javaHome vmArgs classpath main appArgs)
  (custom-set-variables
   '(semanticdb-default-save-directory "/tmp")

   '(jde-project-context-switching-enabled-p nil)

   '(jde-run-classic-mode-vm t)
   '(jde-db-classic-mode-vm t)
   '(jde-bug-debugger-command-timeout 5)
   '(jde-db-log-debugger-output-flag t)

   `(jde-jdk-registry (quote (("1.4.0" . ,javaHome))))
   '(jde-jdk (quote ("1.4.0")))
   '(jde-expand-classpath-p nil)

   `(jde-run-working-directory ,wd)

   ;; Specify classpath.
   ;; Needs: (quote ("/export/home/carr/.sync/.lsync/lava/.classes"))
   `(jde-global-classpath (quote ,(hcJde-normalizePath classpath)))

   ;; Set source package roots so emacs can step into source.
   ;; Needs: (quote ("/export/home/carr/.sync/.lsync/lava")))
   `(jde-sourcepath (quote ,(hcJde-normalizePath sourcePath)))

   ;; Specify the debugger.
   (cond ((eq *hcjdb* 'JDEbug)
	  '(jde-debugger (quote ("JDEbug"))))
	 ((eq *hcjdb* 'jdb)
	  '(jde-debugger (quote ("jdb"))))
	 (t (error "Unknown")))

   ;; Specify args (other than classpath) to VM and debugger.
   ;; Needs: (quote ("-Duser.home=/export/home/carr"))
   ;;`(jde-run-option-vm-args (quote (,vmArgs)))
   `(jde-db-option-vm-args (quote ,(split-string vmArgs " ")))
   `(jde-run-application-class ,main)
   ;; Needs: (quote ("1" "two")))
   ;;`(jde-run-option-application-args (quote (,appArgs)))
   `(jde-db-option-application-args (quote ,(split-string appArgs " ")))
   )
  ;; Start it
  (cond (t 
	 (jde-bug-start-debugger)
	 )
	((eq *hcjdb* 'JDEbug)
	 (jde-debug))
	((eq *hcjdb* 'jdb)
	 (jde-jdb))
	(t (error "Unknown")))
)

;;;;
;;;; RIP Stuff.
;;;;

(hcShDefCmd ripOrbdArgs           ())
(hcShDefCmd ripOrbdMainClass      ())
(hcShDefCmd ripTnameservArgs      ())
(hcShDefCmd ripTnameservMainClass ())
(hcShDefCmd ripClientArgs         ())
(hcShDefCmd ripServerArgs         ())

;;;
;;; Just give this routine the main class and any args and go!
;;;

(defun hcJdeRip-shellWithWorkingDir (workingDir main argsToMain)
  (hcJdeRip-ioser)
  (hcJde-debug
   ;; Directory to start in.
   workingDir
   (hcJdeRipCommon-sourcePathlist (BOOTDIR)
				  (ripHomeColon) (ripPlatform) (hcPathSep))
   (BOOTDIR)
   (hcJdeRipCommon-vmArgs         (BOOTDIR)
				  (ripHomeColon) (ripPlatform) (hcPathSep))
   (hcJdeRipCommon-classpath      (BOOTDIR) 
				  (ripHomeColon) (ripPlatform) (hcPathSep))
   main
   argsToMain))

(defun hcJdeRip-shell (main argsToMain)
  (hcJdeRip-shellWithWorkingDir
   (concat (expand-file-name ".")) main argsToMain))

;;;;
;;;; Utilities.
;;;;

(defun hcJde-normalizePath (path)
  (if (listp path) 
      path
    (split-string path (hcPathSep))))

;; REVISIT: Not sure this works or is even necessary.
(defun hcJdeRip-ioser ()
  (let ((platform (ripPlatform))
	(ripHome  (ripHomeColon))
	(ps       (hcPathSep)))
    ;; Prepare JVM to find ioser.
    (cond ((equal platform "win32")
	   (setenv "PATH" 
		   (concat ripHome "/build/" platform "/bin" ps
			   (getenv "PATH"))))
	  (t
	   (let ((LD (concat ripHome "/build/" platform "/lib/sparc")))
	     (setenv "LD_LIBRARY_PATH"
		     (concat LD ps (getenv "LD_LIBRARY_PATH"))))))))
	     
(provide 'hcJde)

(comment
(load-file "hcJde.el")
)

;;; End of file.

