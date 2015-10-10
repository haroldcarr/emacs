;;; package --- init lisps

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.el$"      . lisp-interaction-mode))

(add-to-list 'auto-mode-alist '("\\.cl$"      . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.kawa$"    . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.llavarc$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lva$"     . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$"    . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.lsp$"     . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$"     . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.silk$"    . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.slk$"     . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.skij$"    . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.skj$"     . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.stk$"     . scheme-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME setup:
;; http://riddell.us/etutorial/slime_swank/slime_swank.html

;; JSWAT usage:
;; http://bc.tech.coop/blog/081023.html
;; http://groups.google.com/group/clojure/browse_thread/thread/403e593c86c2893f
;; /System/Library/Frameworks/JavaVM.framework/Versions/1.5/Home/
;; /System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/
;; /usr/local/hc/java/jswat/jswat-4.3/bin/jswat -jdkhome /System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/ &
;; (slime-connection-port (slime-connection))
;;

(comment
java -server \
     -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8888 \
     -jar /usr/local/hc/java/clojure/clojure_20090320/clojure.jar &
)

(cond (nil ;;(not (hcXEmacsP))
       ;; clojure-mode
       (add-to-list 'load-path (concat (hcUlhcd) "/java/clojure/emacs/clojure-mode"))
       (require 'clojure-mode)

       ;; swank-clojure
       (add-to-list 'load-path (concat (hcUlhcd) "/java/clojure/emacs/swank-clojure"))
       (require 'swank-clojure-autoload)
       (swank-clojure-config
	;;(setq swank-clojure-jar-path (concat (hcUlhcd) "/java/clojure/clojure_20090320/clojure.jar"))
	(setq swank-clojure-binary (concat (hcEsync) "/bin/hcClojure"))
	;;(setq swank-clojure-extra-classpaths (list (concat (hcHome) "/.clojure/clojure-contrib.jar")))
	)

       ;; slime
       (eval-after-load "slime"
	 '(progn (slime-setup '(slime-repl))))

       (add-to-list 'load-path (concat (hcUlhcd) "/java/clojure/emacs/slime"))
       (require 'slime)
       (slime-setup)
      )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hcRunCommand)

;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hcPomCommand)

;;;;;;;;;;;;;;;;;;;;;;;;;
(hcRunCommand clisp hcClispCmd)

(defun hcClispCmd ()
  (cond ((hcWin32P)
	 (concat
	  (hcUlhcd) "/lisp/clisp/clisp-1999-07-22/lisp -M "
	  (hcUlhcd) "/lisp/clisp/clisp-1999-07-22/lispinit.mem"))
	((hcDarwinP)
	 ;;"clisp"
	 "/sw/src/clisp-2.33.2-1/clisp-2.33.2/src/clisp"
	 )
	(t
	 (concat
	  (hcUlhcd) "/lisp/clisp/clisp-1999-01-08/base/lisp.run -M "
	  (hcUlhcd) "/lisp/clisp/clisp-1999-01-08/base/lispinit.mem"))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand clocc hcCloccCmd)

(defun hcCloccCmd ()
  (if (hcDarwinP)
      (concat "clisp" " -M "
	      " /Volumes/User/sw/lib/clocc/clocc-01-18-04/clocc/clocc-top.mem")
    (error "only configured for darwin")))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand sbcl hcSbclCmd)

(defun hcSbclCmd ()
    (if (hcDarwinP)
      (concat
       (hcUlhcd) "/lisp/sbcl/sbcl-0.8.2.7/src/runtime/sbcl --core "
       (hcUlhcd) "/lisp/sbcl/sbcl-0.8.2.7/output/sbcl.core")
      (error "only configured for darwin")))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand j hcJCmd)

(defun hcJCmd ()
  (concat
   (HC-JAVA_HOME-bin) "/java "
   " -classpath "
   (hcUlhcd) "/java/j/j-0.21.0/j.jar"
   (hcPathSep)
   (HC-JAVA_HOME-classes)
   (hcPathSep)
   (hcLibClasspath)
   " "
   " -Xss512K "
   " "
   " org.armedbear.lisp.Main"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand jscheme hcJschemeCmd)

(defun hcJschemeCmd ()
  (concat
   (HC-JAVA_HOME-bin) "/java "
   *compile-threshold*
   " -jar "
   (hcUlhcd) "/java/jscheme/jscheme_6_1.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand kawa hcKawaCmd)

(defun hcKawaCmd ()
  (concat
   (HC-JAVA_HOME-bin) "/java "
   *compile-threshold*
   " -classpath "
   (hcUlhcd) "/java/kawa/kawa-1.7.jar"
   (hcPathSep)
   (HC-JAVA_HOME-classes)
   (hcPathSep)
   (hcLibClasspath)
   " "
   " kawa.repl"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcShDefCmd hcLlavaClasspath ())
(hcShDefCmd ripClasspath (javaHome rmiIiopHome))

;;; Hooks so you can add to classpath, vmargs and command line args.

(defun hcLlavaCmdEnvExtras       (ps) "")
(defun hcLlavaCmdClasspathExtras (ps) "")
(defun hcLlavaCmdVmArgsExtras    (ps) "")
(defun hcLlavaCmdMainClass       (ps) (hcLlavaMainClass))
(defun hcLlavaCmdLineArgsExtras  (ps) "")

;;------

(hcRunCommand llava hcLlavaCmd)

(defun hcLlavaCmd ()
  (hcLlavaStartCmd (hcLlavaCmdMainClass (hcPathSep))))

;;------

(hcRunCommand jllava hcJLlavaCmd)

(defun hcJLlavaCmd ()
  (hcLlavaStartCmd
   (concat "-jar " (hcSync) "/.llava.org/.system/jars/llava.jar")))

;;------

(hcRunCommand dljllava hcDLJLlavaCmd)

(defun hcDLJLlavaCmd ()
  (hcLlavaStartCmd
   (concat "-jar " (hcFtptmp) "/llava.jar")))

(defun hcLlavaStartCmd (startup)
  (concat
   (hcLlavaCmdEnvExtras (hcPathSep))
   " "
   (HC-JAVA_HOME-bin) "/java "
   *compile-threshold*
   "-Xmx2024m"
   " -classpath "
   (hcLlavaCmdClasspathExtras (hcPathSep))
   (hcPathSep)
   "."
   (hcPathSep)
   (hcLlavaClasspath)
   (hcPathSep)
   (HC-JAVA_HOME-classes)
   (hcPathSep)
   (hcLibClasspath)
   (hcPathSep)
   " "
   " -Duser.home=" (hcHome)
   " "
;;   " -Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=y,suspend=y "
   " "
   (hcLlavaCmdVmArgsExtras (hcPathSep))
   " "
   startup
   " "
   (hcLlavaCmdLineArgsExtras (hcPathSep))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcShDefCmd hcSkijClasspath ())

(hcRunCommand skij hcSkijCmd)

(defun hcSkijCmd ()
  (concat
   (HC-JAVA_HOME-bin) "/java "
   " -classpath "
   "."
   (hcPathSep)
   (hcSkijClasspath)r
   (hcPathSep)
   (HC-JAVA_HOME-classes)
   (hcPathSep)
   (hcLibClasspath)
   (hcPathSep)
   (hcLlavaCmdClasspathExtras (hcPathSep))
   " "
   " -Duser.home=" (hcHome)
   " "
   (hcLlavaCmdVmArgsExtras (hcPathSep))
   " "
   " com.ibm.jikes.skij.Scheme"
   " "
   (hcLlavaCmdLineArgsExtras (hcPathSep))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand silk hcSilkCmd)

(defun hcSilkCmd ()
  (concat
   (HC-JAVA_HOME-bin) "/java "
   " -classpath "
   "."
   (hcPathSep)
   (hcUlhcd) "/java/silk/v3.0-99-10-31/silk/jar/scheme.jar"
   (hcPathSep)
   (HC-JAVA_HOME-classes)
   (hcPathSep)
   (hcUlhcd) "\\java\\jdk1.2.1\\jre\\lib\\tools.jar"
   (hcPathSep)
   (hcLibClasspath)
   (hcPathSep)
   (hcLlavaCmdClasspathExtras (hcPathSep))
   " "
   " -Duser.home=" (hcHome)
   " "
   (hcLlavaCmdVmArgsExtras (hcPathSep))
   " "
   " silk.Scheme generic/load.scm"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand MzScheme hcMzSchemeCmd)

(defun hcMzSchemeCmd ()
  (concat (hcUlhcd) "/lisp/plt/202/plt/bin/mzscheme"))

(provide 'hcInitLisps)
;;; hcInitLisps.el ends here

