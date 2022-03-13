;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "LISP and Scheme and Clojure - external")

;; SLIME setup:
;; http://riddell.us/tutorial/slime_swank/slime_swank.html

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
       (add-to-list 'load-path "~/hc/java/clojure/emacs/clojure-mode")
       (require 'clojure-mode)

       ;; swank-clojure
       (add-to-list 'load-path "~/hc/java/clojure/emacs/swank-clojure")
       (require 'swank-clojure-autoload)
       (swank-clojure-config
	;;(setq swank-clojure-jar-path "~/hc/java/clojure/clojure_20090320/clojure.jar")
	(setq swank-clojure-binary "~/.sync/.esync/bin/hcClojure")
	;;(setq swank-clojure-extra-classpaths (list "~/.clojure/clojure-contrib.jar"))
	)

       ;; slime
       (eval-after-load "slime"
	 '(progn (slime-setup '(slime-repl))))

       (add-to-list 'load-path "~/hc/java/clojure/emacs/slime")
       (require 'slime)
       (slime-setup)
      )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
(hcRequire hcRunCommand)

;;;;;;;;;;;;;;;;;;;;;;;;;
(hcRequire hcPomCommand)

;;;;;;;;;;;;;;;;;;;;;;;;;
(hcRunCommand clisp hcClispCmd)

(defun hcClispCmd ()
  (cond ((hcWin32P)
	 (concat
	  (usrLocalHcDir) "/lisp/clisp/clisp-1999-07-22/lisp -M "
	  (usrLocalHcDir) "/lisp/clisp/clisp-1999-07-22/lispinit.mem"))
	((hcDarwinP)
	 ;;"clisp"
	 "/sw/src/clisp-2.33.2-1/clisp-2.33.2/src/clisp"
	 )
	(t
	 (concat
	  (usrLocalHcDir) "/lisp/clisp/clisp-1999-01-08/base/lisp.run -M "
	  (usrLocalHcDir) "/lisp/clisp/clisp-1999-01-08/base/lispinit.mem"))))

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
       (usrLocalHcDir) "/lisp/sbcl/sbcl-0.8.2.7/src/runtime/sbcl --core "
       (usrLocalHcDir) "/lisp/sbcl/sbcl-0.8.2.7/output/sbcl.core")
      (error "only configured for darwin")))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand j hcJCmd)

(defun hcJCmd ()
  (concat
   (BOOTDIR-bin) "/java "
   " -classpath "
   (usrLocalHcDir) "/java/j/j-0.21.0/j.jar"
   (hcPathSep)
   (BOOTDIR-classes)
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
   (BOOTDIR-bin) "/java "
   *compile-threshold*
   " -jar "
   (usrLocalHcDir) "/java/jscheme/jscheme_6_1.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand kawa hcKawaCmd)

(defun hcKawaCmd ()
  (concat
   (BOOTDIR-bin) "/java "
   *compile-threshold*
   " -classpath "
   (usrLocalHcDir) "/java/kawa/kawa-1.7.jar"
   (hcPathSep)
   (BOOTDIR-classes)
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
   (concat "-jar "
	   (hcExpandFileName t "~") "/.sync/.llava.org/.system/jars/llava.jar")))

;;------

(hcRunCommand dljllava hcDLJLlavaCmd)

(defun hcDLJLlavaCmd ()
  (hcLlavaStartCmd
   (concat "-jar "
	   (hcExpandFileName t "~") "/ftptmp/llava.jar")))

(defun hcLlavaStartCmd (startup)
  (concat
   (hcLlavaCmdEnvExtras (hcPathSep))
   " "
   (BOOTDIR-bin) "/java "
   *compile-threshold*
   "-Xmx2024m"
   " -classpath "
   (hcLlavaCmdClasspathExtras (hcPathSep))
   (hcPathSep)
   "."
   (hcPathSep)
   (hcLlavaClasspath)
   (hcPathSep)
   (BOOTDIR-classes)
   (hcPathSep)
   (hcLibClasspath)
   (hcPathSep)
;;   (ripClasspath (BOOTDIR) (ripHomeColon))
   " "
   " -Duser.home=" (hcExpandFileName t "~")
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
   (BOOTDIR-bin) "/java "
   " -classpath "
   "."
   (hcPathSep)
   (hcSkijClasspath)
   (hcPathSep)
   (BOOTDIR-classes)
   (hcPathSep)
   (hcLibClasspath)
   (hcPathSep)
;;   (ripClasspath (BOOTDIR) (ripHomeColon))
   (hcLlavaCmdClasspathExtras (hcPathSep))
   " "
   " -Duser.home=" (hcExpandFileName t "~")
   " "
   (hcLlavaCmdVmArgsExtras (hcPathSep))
   " "
   " com.ibm.jikes.skij.Scheme"
   " "
   (hcLlavaCmdLineArgsExtras (hcPathSep))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand skijMs hcSkijMsCmd)

(defun hcSkijMsCmd ()
  (concat
   *hc-microsoft-java-classes*
   *usrLocalHcDirDrive* "\\usr\\local\\java\\skij\\jar;"
   "c:\\winnt\\java\\trustlib\\hello32\\classes;"
   "c:\\winnt\\java\\classes\\classes.zip;"
   "d:\\ESSENCEOFOLE\\chap15\\javahelloauclient;"
   " com.ibm.jikes.skij.Scheme"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand silk hcSilkCmd)

(defun hcSilkCmd ()
  (concat
   (BOOTDIR-bin) "/java "
   " -classpath "
   "."
   (hcPathSep)
   "d:/usr/local/hc/java/silk/v3.0-99-10-31/silk/jar/scheme.jar"
   (hcPathSep)
   (BOOTDIR-classes)
   (hcPathSep)
   "D:/usr/local/hc\\java\\jdk1.2.1\\jre\\lib\\tools.jar"
   (hcPathSep)
   (hcLibClasspath)
   (hcPathSep)
;;   (ripClasspath (BOOTDIR) (ripHomeColon))
   (hcLlavaCmdClasspathExtras (hcPathSep))
   " "
   " -Duser.home=" (hcExpandFileName t "~")
   " "
   (hcLlavaCmdVmArgsExtras (hcPathSep))
   " "
   " silk.Scheme generic/load.scm"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand MzScheme hcMzSchemeCmd)

(defun hcMzSchemeCmd ()
  (concat (usrLocalHcDir) "/lisp/plt/202/plt/bin/mzscheme"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(hcRunCommand hcTelnetClient hcTelnetClientCmd)

(defun hcTelnetClientCmd ()
  (concat
   (BOOTDIR-bin) "/java "
   " -classpath "
   (hcExpandFileName t "~/.sync/.esync/java/.classes")
   (hcPathSep)
   (usrLocalHcDir) "/java/jta/jta20/.classes"
   " "
   " hc.net.TelnetClient localhost 4444"))
