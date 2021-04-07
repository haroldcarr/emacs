;;; .emacs.vanilla --- init file         -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; two flycheck warnings : 1st I understand; no idea where 2nd comes from
;; 57   1 warning         cl package required at runtime (emacs-lisp)
;; 734   1 warning         the following functions might not be defined at runtime: neo-buffer--unlock-width, neo-buffer--lock-width (emacs-lisp)

(defvar hc-emacs "hcev")

;; ------------------------------------------------------------------------------
;; * SETUP

;; (hcSection "Packages")

;;- [[http://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html#Packages]]
;;- [[http://emacswiki.org/emacs/ELPA]]

;; ELPA does not update the load path when new packages are installed.
;; This will do that.

(defvar package-alist)
(defun package-update-load-path ()
  "Update the load path for newly installed packages."
  (interactive)
  (let ((package-dir (hcExpandFileName nil package-user-dir)))
    (mapc (lambda (pkg)
            (let ((stem (symbol-name (car pkg)))
		  (version "")
		  (first t)
		  path)
	      (mapc (lambda (num)
		      (if first
			  (setq first nil)
			  (setq version (format "%s." version)))
		      (setq version (format "%s%s" version num)))
		    (aref (cdr pkg) 0))
              (setq path (format "%s/%s-%s" package-dir stem version))
              (add-to-list 'load-path path)))
          package-alist)))

(require 'package)
;; TODO: Other code uses .emacs.d too.
;;       For now I have a symlink from ~/.emacs.d to here.
;; (setq package-user-dir (concat (hcEmacsDir) "/.emacs.d/elpa"))
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ("gnu"   . "http://elpa.gnu.org/packages/")))

;;        ("marmalade"    . "http://marmalade-repo.org/packages/")
;;        ("melpa-stable" . "http://stable.melpa.org/packages/")

(package-initialize)

;; can't get rid of warning
;; https://stackoverflow.com/questions/5019724/in-emacs-what-does-this-error-mean-warning-cl-package-required-at-runtime
(eval-and-compile
  (require 'cl))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'bind-key))

(require 'diminish nil t)

(add-to-list 'load-path (shell-command-to-string "hcLocation emacs"))

(require 'use-package)
(require '.emacs.common)


;; ------------------------------------------------------------------------------
;; * named keyboard macros

(fset 'ecp
   (kmacro-lambda-form [?\C-s ?\[ ?l ?a ?b ?e ?l ?= ?\" ?E ?C ?P ?\C-e backspace backspace backspace ?\C-d ?\C-d ?\C-d] 0 "%d"))

;; ------------------------------------------------------------------------------
;; * Top level misc

(hcSection "Top level misc stuff")

(when (member (hcMachineName) '("hcarr-mac"))
  (desktop-save-mode 1))

;; Store customizations in a separate file.
(setq custom-file (concat (hcEmacsDir) "/.vanilla.emacs.custom.el"))
(load custom-file)

(load-file (concat (hcEmacsDir) "/hc-mode-line.el"))

;; WHICH KEY
(use-package which-key
  :ensure t
  :demand
  :pin melpa
  :init (which-key-mode))

;; rather than highlight or off
(setq blink-matching-paren (quote jump))

;; Simplify prompts.
(fset 'yes-or-no-p 'y-or-n-p)

;; Get rid of useless stuff.
(dolist (mode '(tool-bar-mode)) ;; menu-bar-mode scroll-bar-mode
  (when (fboundp mode) (funcall mode -1)))

;; I have already seen it.
(setq inhibit-startup-screen t)

;; Do not want to destroy symbolic links.
(setq backup-by-copying-when-linked t)

;; Enable ESC : to evaluate emacs Lisp commands.
(put 'eval-expression 'disabled nil)

;; ** Mode line

;; http://www.emacswiki.org/emacs/DisplayTime

(load "time")
(display-time)

(line-number-mode)
(column-number-mode)

;; ** Make buffer names unique

;; Use part of the path name for buffer name when visiting two different files with same name.

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html#Uniquify
;; http://emacswiki.org/emacs/uniquify

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator ":"))

;; ------------------------------------------------------------------------------
;; * Naviation

(hcSection "navigation (ido | helm")

;; ** Incremental/Interactively switching buffers or finding files

;; IDO
;; http://emacswiki.org/emacs/InteractivelyDoThings

;; Note:
;; - C-x C-f     : "interactively" finds a file
;; - C-x C-f C-f : uses the old find-file

;(use-package ido
;  :ensure t
;  :config
;  (ido-mode t)
;  (setq ido-enable-flex-matching t))

;; TODO
;; http://www.emacswiki.org/emacs-en/kill-ring-ido.el

;; HELM
(use-package hc-helm)

;; ** Recently visited files

;; TODO
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/

;; ------------------------------------------------------------------------------
;; * completion

(hcSection "completion (company)")

(use-package hc-company)

;; ------------------------------------------------------------------------------
;; * yasnippet

(hcSection "yasnippet")

(use-package hc-yasnippet)

;; ------------------------------------------------------------------------------
;; * daemon / emacsclient

(hcSection "daemon / emacsclient")

;; See (I could not get C-c e to work in IntelliJ)
;; http://spin.atomicobject.com/2014/08/07/intellij-emacs/

;; # manual start
;; ./bin-hosted/emacs --daemon
;; # manual use
;; ./bin-hosted/emacsclient -c <any file/dir name>
;; # manual kill
;; # - from within emacs
;; M-x kill-emacs
;; or
;; M-x save-buffers-kill-emacs
;; # from outside of emacs
;; emacsclient -e '(kill-emacs)'
;; or
;; emacsclient -e '(client-save-kill-emacs)'

(require 'server)

(defun server-started-p ()
  "Return non-nil if this Emacs has a server started."
  (and (boundp server-process) server-process))

(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24)
               (not (server-started-p)))
      (server-start)))

;; ------------------------------------------------------------------------------
;; * Projectile

(use-package hc-projectile)

;; ------------------------------------------------------------------------------
;; Bookmarks

;;(hcSection "Bookmarks")
;;
;;(use-package hc-bookmark-plus)

;; ------------------------------------------------------------------------------
;; * Emacs Code Browser (ECB)

;; http://www.emacswiki.org/emacs/EmacsCodeBrowser

;; - Go to directories : "C-c . g d" ;; window 0
;; - Go to sources     : "C-c . g s" ;; window 1
;; - Go to methods     : "C-c . g m" ;; window 2
;; - Go to history     : "C-c . g h" ;; window 3
;; - Main buffer       : "C-c . g 1"

(hcSection "ECB")

(defvar ecb-options-version)
(defvar ecb-source-path)
(defvar ecb-tip-of-the-day)
(defvar ecb-windows-width)
(defvar ecb-layout-name)
(use-package ecb
  :defer t
  :config
  (setq ecb-options-version "2.40")
  (setq ecb-source-path     '(("/" "/")))
  (setq ecb-tip-of-the-day  nil)
  (setq ecb-windows-width   0.15)
  (setq ecb-layout-name     "left9") ;; only the methods window
)

;; ------------------------------------------------------------------------------
;; * Compilation

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Building.html#Building
;; - see Compilation, Compilation Mode and Compilation Shell
;; http://emacswiki.org/emacs/CompilationMode

;; For extending it to work with Maven:

;; http://praveen.kumar.in/2011/03/09/making-gnu-emacs-detect-custom-error-messages-a-maven-example/

(hcSection "Compilation")

(use-package compile
  :defer t
  :config
  (add-to-list 'compilation-error-regexp-alist 'maven)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(maven "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
                       1 2 3)))

;; ------------------------------------------------------------------------------
;; * Calendar and Diary

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html#Calendar_002fDiary
;; http://emacswiki.org/emacs/CalendarMode

(hcSection "Calendar and Diary")

(defvar view-diary-entries-initially)
(defvar number-of-diary-entries)
(defvar calendar-latitude)
(defvar calendar-longitude)
(defvar diary-list-include-blanks)
(defun hcCalendar () "."
  (setq diary-file (concat (hcFsync) "/.emacs.diary"))
  ;(setq calendar-week-start-day 1) ; monday
  (setq calendar-offset -1)
  (setq view-diary-entries-initially t)
  (setq number-of-diary-entries 2)
  ;; This causes a debug error in emacs 24
  ;(setq mark-diary-entries-in-calendar t)
  ;; This causes fancy not to be displayed
  ;;(setq view-calendar-holidays-initially t)
  ;;(setq holidays-in-diary-buffer nil)
  (setq calendar-latitude  40.785188)
  (setq calendar-longitude -111.863011)

  (add-hook 'diary-display-hook 'fancy-diary-display)
  (setq diary-list-include-blanks t)
  ;; not working: (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
  ;; Make sure starting calendar comes after auto-resizing of frame.
  ;; (if (hcLucidP)
  ;;    (defun diary-remind (form number)
  ;;      (eval form)))
  ;; Do not automatically show calendar on startup
  ;(if (file-exists-p diary-file)
  ;    (calendar))
  ;; Customize appt.el
  ;;(use-package appt :defer t)
  (cond (nil
	 ;; This seems to cause emacs to crash on nt
	 ;; and does not exist in xemacs
	 (setq appt-message-warning-time 15) ;; minutes
	 (setq appt-display-duration 60) ;; seconds
	 (add-hook 'diary-hook 'appt-make-list)
	 (let ((diary-display-hook 'ignore))
	   (diary)))))

(use-package calendar :defer t :config (hcCalendar))

;; ** Calendar Framework

;; https://github.com/kiwanami/emacs-calfw

;; M-x cfw:open-diary-calendar
(use-package calfw-cal :defer t)

;; ------------------------------------------------------------------------------
;; * Line Numbers

;; http://www.emacswiki.org/LineNumbers

(hcSection "Line Numbers")

(use-package linum :defer t
;;  :config (setq global-linum-mode t) ;; always on
)

;; ------------------------------------------------------------------------------
;; * Haskell

(when (member (hcMachineName) '("hcmb" "o2020" "hcarr-mac"))
  (hcSection "Haskell")
  (use-package hc-haskell))

;;(use-package intero
;;  :config (progn (add-hook 'haskell-mode-hook 'intero-mode)
;;                 ;; https://github.com/commercialhaskell/intero/issues/208
;;                 (setq flycheck-check-syntax-automatically '(mode-enabled save))))

;; ------------------------------------------------------------------------------
;; * Agda

(when (member (hcMachineName) '("hcmb" "o2020" "hcarr-mac"))
  (hcSection "Agda")
  (use-package hc-agda))

;; ------------------------------------------------------------------------------
;; * Images

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html

(hcSection "Images")

(defvar image-dired-dir)
(setq image-dired-dir "/tmp/emacs-image-dired/")

;; ------------------------------------------------------------------------------
;; * Align

;; https://gist.github.com/700416
;; http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
;; - uses http://www.emacswiki.org/emacs/rx

(hcSection "Align")

(defmacro hcMakeAlignCmd (name char) "NAME CHAR."
  `(defun ,name (begin end)
     ,(concat "Align region to " char " signs")
     (interactive "r")
     (align-regexp begin end
                   (rx (group (zero-or-more (syntax whitespace))) ,char)
                   1 1)))

(hcMakeAlignCmd align-to-colon        ":")
(hcMakeAlignCmd align-to-equals       "=")
(hcMakeAlignCmd align-to-hash         "=>")
(hcMakeAlignCmd align-to-comma-before ",")

(defun align-to-comma-after (begin end)
  "BEGIN END: Align region to , signs."
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

;; ------------------------------------------------------------------------------
;; * Google Search

;; http://emacsredux.com/blog/2013/03/28/google/

(hcSection "Google Search")

(defun hcGoogle ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url ;; results in default browser
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; ------------------------------------------------------------------------------
;; * Google Contacts

;; http://julien.danjou.info/projects/emacs-packages#google-contacts

(hcSection "Google Contacts")

;; uses oauth2 (in ELPA)

;; M-x google-contacts

;; key bindings:
;; n or p : go the next or previous record;
;; g      : refresh the result, bypassing the cache;
;; m      : send an e-mail to a contact;
;; s      : new search;
;; q      : quit.
(use-package google-contacts :defer t)

;; integrate directly Google Contacts into Gnus;
;; (use-package google-contacts-gnus :defer t)
;; Then use ; to go to contact info while reading an e-mail.

;; integrate directly Google Contacts into message-mode;
;; (use-package google-contacts-message :defer t)
;; Then use TAB to go to complete e-mail addresses in the header fields.

;; First time use:
;; - M-x google-contacts
;; - "Enter the code your browser displayed: "
;; - browser shows accounts.google.com/... saying
;;   google-oauth-el would like to manage your contacts
;;   CLICK: Accept
;; - Gives code.
;; - Cut/paste into above.
;; - "Passphrase for PLSTORE  plstore .../.emacs.d/oauth.plstore
;; - enter and store in password manager"

;; ------------------------------------------------------------------------------
;; * Google Maps

;; http://julien.danjou.info/projects/emacs-packages#google-maps

;; M-x google-maps
;; - type a location.
;;
;; key bindings:
;;
;; + or - to zoom in or out;
;; left, right, up, down to move;
;; z to set a zoom level via prefix;
;; q to quit;
;; m to add or remove markers;
;; c to center the map on a place;
;; C to remove centering;
;; t to change the maptype;
;; w to copy the URL of the map to the kill-ring;
;; h to show your home.

;; Integrate into Org-mode:

(use-package org-location-google-maps :defer t)

;; Then use C-c M-L to enter a location assisted by Google geocoding service.
;; Pressing C-c M-l will show you a map.

;; Advanced: look at google-maps-static-show and google-maps-geocode-request functions.

;; NOTE: home set via calendar-latitude/calendar-longitude

(hcSection "Google Maps")
(use-package google-maps :defer t)

;; ------------------------------------------------------------------------------
;; * Twitter

;; http://www.emacswiki.org/emacs/TwitteringMode

(hcSection "Twitter")
(use-package twittering-mode :defer t)

;; ------------------------------------------------------------------------------
;; * Misc

(hcSection "Misc")
(use-package httpcode :defer t)

(defun hc-tps (ntx ndays nhours nminutes nseconds)
  (let ((days-seconds    (* (* (* ndays 24) 60) 60))
        (hours-seconds   (* (* nhours       60) 60))
        (minutes-seconds (* nminutes            60)))
    (/ ntx (+ days-seconds hours-seconds minutes-seconds nseconds))))

;; ------------------------------------------------------------------------------
;; * Features used but not customized

;; ** Dired
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html#Dired
;; - http://emacswiki.org/emacs/DiredMode
;; ** Dynamic Abbreviations
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html#Dynamic-Abbrevs
;; - http://emacswiki.org/emacs/DynamicAbbreviations
;; ** Speedbar
;; - http://www.gnu.org/software/emacs/manual/html_mono/speedbar.html
;; - http://emacswiki.org/emacs/SpeedBar
;; ** Keyboard Macros
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macros.html#Keyboard-Macros
;; - http://emacswiki.org/emacs/KeyboardMacros
;; ** Document Viewing
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Document-View.html#Document-View
;; - http://www.emacswiki.org/emacs/DocViewMode
;; ** Color Themes and rainbow-mode
;; - http://emacsredux.com/blog/2013/08/21/color-themes-redux/
;; - http://julien.danjou.info/projects/emacs-packages#rainbow-mode
;; ** DIG (interface to DNS dig command)
;; - /Applications/MacPorts/Emacs.app/Contents/Resources/lisp/net/dig.el.gz
;; - http://stuff.mit.edu/afs/athena/astaff/project/emacs/source/emacs-23.1/lisp/net/dig.el

;; ------------------------------------------------------------------------------
;; * Non Literate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XML/HTML
(defvar sgml-basic-offset)
(setq sgml-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "Open current buffer's associated file in an external program")

(defun prelude-open-with ()
  "To open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open -a '/Applications/Google Chrome.app'"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "Java")

(defvar *hcJavaMode* 'not-google)
(declare-function  google-set-c-style ".")
(add-hook 'java-mode-hook
  (lambda () (if (eq *hcJavaMode* 'google) (google-set-c-style))))

;; M-x google-set-c-style
(use-package google-c-style)

;; Make java mode support Java 1.5 annotations.
(declare-function auto-complete-mode ".")
(use-package java-mode-indent-annotations
  :config
  (add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
  (add-hook 'java-mode-hook (lambda () (auto-complete-mode 1))))

(defvar *compile-threshold*)
(setq *compile-threshold* " -XX:CompileThreshold=2 ")

(defun HC-JAVA_HOME () "."
  (cond ((getenv "JAVA_HOME"))
	(t (error "No default JDK"))))
(defun HC-JAVA_HOME-bin     () "." (concat (HC-JAVA_HOME) "/bin"))
(defun HC-JAVA_HOME-classes () "." (concat (HC-JAVA_HOME) "/jre/lib/rt.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "Scala")

(defvar scala-indent:step)
(use-package scala-mode2
  :defer t
  :config
  (setq scala-indent:step 4))

;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "Clojure")

(defvar clojure-enable-fancify-symbols)
(use-package cider-mode
  :defer nil ;; HC so cider can find 'clojure-project-dir'
;; :pin melpa-stable
  :config
  (progn
    ;;(define-key cider-mode-map (kbd "C-c C-e") #'hc-cider-insert-last-sexp-in-repl)
    (setq clojure-enable-fancify-symbols t)
    ))

(use-package cider
  :defer t
;;  :pin melpa-stable
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "LISP and Scheme and Clojure")

(use-package hc-lisps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "C")

;;(load "c-mode")

(defvar c-indent-level)
(setq c-indent-level 4)
;(setq c-continued-statement-offset 4)
;(setq c-brace-offset -4)
;(setq c-argdecl-indent 4)
;(setq c-label-offset -2)

;;(load "c++-mode")

(add-to-list 'auto-mode-alist '("\\.idl$"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c$"    . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh$"   . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "Appearance")

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;This turns of the mac os x menu bar
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defun hcMacFW () "."
  (interactive)
  (hc-w 88) ;; 100
  (hc-h 23) ;;  27                                              20
  (set-face-font 'default "-apple-Monaco-medium-normal-normal-*-24-*-*-*-m-0-iso10646-1")
  )

(defun hcColor () "."
   (interactive)
   (set-foreground-color "#DCDCCC")
   (set-background-color "#3F3F3F")
   (set-cursor-color     "#FFFFEF"))

(defun hcHostedAppearance () "."
  (interactive)
  ;;(set-face-background 'default "grey")
  ;;(set-face-background 'default "gray19")
  ;;(set-face-background 'default "gray14")
  ;;(set-face-background 'default "gray10")
  (set-scroll-bar-mode 'right)
  ;;(set-face-font 'default "-unknown-DejaVu LGC Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (set-face-font 'default "-PfEd-TlwgTypewriter-normal-normal-normal-*-24-*-*-*-*-0-iso10646-1")
  ;;(hc-w 160)
  ;;(hc-h 43)
  )

;; type fc-list on command line to see fonts installed

;;(set-face-attribute 'default nil :family "courier" :height 140)
;;(set-face-font 'default "-*-Lucida Sans Typewriter-Medium-R-*-*-*-200-*-*-*-*-iso8859-1")
;;(set-face-font 'modeline "-*-Lucida Sans Typewriter-medium-R-*-*-*-200-*-*-*-*-iso8859-1")

;;; Background
;;(set-face-background 'default "#9900991b99fe") ; grey
;;(set-face-background 'default "#900060006000") ; earthy red
;;(set-face-background 'default "#de00b8008700") ; earthy orange
;;(set-face-background 'default "#737373737373") ; grey
;;(set-face-background 'default "#6a6a6a6a6a6a") ; grey
;;(set-face-background 'default "DarkSlateGrey")
;;(set-face-background 'default "grey")
;;(defined-colors)
;; ("snow" "ghost white" "GhostWhite" "white smoke" "WhiteSmoke" "gainsboro" "floral white" "FloralWhite" "old lace" "OldLace" "linen" "antique white" ...)
;;(set-face-background 'default "antique white")
;;(set-face-background 'default "grey99")
;;(set-face-background 'default "White")
;;(set-face-background 'default "#b9b9b9b9b9b9")
;;(set-face-background 'default "#dddddddddddd")
;;(set-face-background 'default "#68006f008200") ; blue
;;(set-face-background 'default "Black")

;;; Foreground
;;(set-face-foreground 'default "Green")
;;(set-face-foreground 'default "DarkSlateGrey")
;;(set-face-foreground 'default "#de00b8008700") ; earthy orange
;;(set-face-foreground 'default "Black")
;;(set-face-foreground 'default "white")

;;; Mark to region.
;;(set-face-background 'primary-selection "grey")
;;(set-face-foreground 'primary-selection "black")))

;;; Incremental search.
;;(set-face-foreground 'isearch "black")
;;(set-face-background 'isearch "green")))

;;; Toolbar.

;; Turn off font-lock?
(defvar font-lock-auto-fontify)
(defvar font-lock-mode-enable-list)
(defun hcFontLockModeHook () "."
  (if (fboundp 'global-font-lock-mode)
      (global-font-lock-mode -1) ;; Emacs
    (setq font-lock-auto-fontify nil))
  (setq font-lock-mode-enable-list nil))

(cond ((hcDarwinP)
       (hcMacFW)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hcSection "Syntax")

;; Make -, * and . letters.

(cond (nil
       (modify-syntax-entry ?- "w" lisp-mode-syntax-table)
       (modify-syntax-entry ?. "w" lisp-mode-syntax-table)
       (modify-syntax-entry ?* "w" lisp-mode-syntax-table)
       (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
       (modify-syntax-entry ?: "w" lisp-mode-syntax-table)))

;; NT: c-mode-syntax-table not defined *****
(cond (nil
       (modify-syntax-entry ?- "w" c-mode-syntax-table)
       (modify-syntax-entry ?. "w" c-mode-syntax-table)
       (modify-syntax-entry ?* "w" c-mode-syntax-table)
       (modify-syntax-entry ?_ "w" c-mode-syntax-table)))

(cond (nil
       (modify-syntax-entry ?- "w" text-mode-syntax-table)
       (modify-syntax-entry ?. "w" text-mode-syntax-table)
       (modify-syntax-entry ?* "w" text-mode-syntax-table)
       (modify-syntax-entry ?_ "w" text-mode-syntax-table)))

(cond (nil
       (modify-syntax-entry ?- "w" (standard-syntax-table))
       (modify-syntax-entry ?. "w" (standard-syntax-table))
       (modify-syntax-entry ?* "w" (standard-syntax-table))
       (modify-syntax-entry ?_ "w" (standard-syntax-table))))

(provide '.vanilla.emacs)

;;; .vanilla.emacs ends here
