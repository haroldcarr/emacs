;;; .emacs.vanilla --- init file         -*- mode: emacs-lisp -*-

;;; Commentary:

;;  1. LOAD personal leim-list.el (in load-path)
;;  2. LOAD ~/.emacs.d/early-init.el — skipped with -q/-Q/--batch                               (Emacs 27+)
;;  3. RUN package-activate-all — skipped with -q/-Q/--batch or package-enable-at-startup = nil (Emacs 27+)
;;  4. RUN before-init-hook                                                                     (Emacs 27+)
;;  5. LOAD site-start.el — skipped with -Q/--no-site-file
;;  6. LOAD ~/.emacs.d/init.el (or ~/.emacs) — skipped with -q/-Q/--batch
;;  7. LOAD default.el — skipped with -q/-Q/--batch/inhibit-default-init
;;  8. LOAD abbrevs from abbrev-file-name
;;  9. RUN after-init-hook
;; 10. RUN delayed-warnings-hook                                                                (Emacs 27+)
;; 11. LOAD terminal-specific library
;; 12. RUN tty-setup-hook
;; 13. RUN emacs-startup-hook                                                                   (Emacs 27+)
;; 14. RUN frame-notice-user-settings                                                           (Emacs 27+)
;; 15. RUN window-setup-hook                                                                    (Emacs 27+)
;; 16. RUN server-start (daemon)

;;; Code:

(defvar hc-emacs "hcev")

(defvar hc-dev-machines '("hc2025" "o2023" "o2020" "o2015" "hcmb-air"))

;; ------------------------------------------------------------------------------
;; STARTUP
;; https://blog.d46.us/advanced-emacs-startup/

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; See END STARTUP below
;; Make startup faster by reducing the frequency of garbage collection.
;; Default is 800 kilobytes.
(setq gc-cons-threshold (* 50 1000000))

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
      '(
        ("melpa"  . "http://melpa.org/packages/")
        ;; https://list.orgmode.org/87lfa7tc9v.fsf@gnu.org/t/
        ("gnu"    . "http://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ;;("org"    . "http://orgmode.org/elpa/")
       ))

;;        ("marmalade"    . "http://marmalade-repo.org/packages/")
;;        ("melpa-stable" . "http://stable.melpa.org/packages/")

(package-initialize)
(setq package-enable-at-startup nil)

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

;; ------------------------------------------------------------------------------
;; BEGIN .emacs.common

;; ------------------------------------------------------------------------------
;; * Sections

;; Name each "section" in this .emacs file.
;;
;; Then, when an error occurs, you can tell what section it occurs in
;; by looking in the *Messages* buffer or examining the *hcSection* variable.
;;
;; debug-on-error is finer grained, but this is still useful.

(defvar *hcSectionEnabled* t)
(defvar *hcSection* "")

(defun hcSection (title)
  "TITLE: For debugging .emacs."
  (cond (*hcSectionEnabled*
         (setq *hcSection* title)
         (message title))))

(defmacro hcSectionLoad (name)
  `(progn
     (hcSection ,(symbol-name name))
     (use-package ,name)))

(setq debug-on-error t)

(defmacro comment (&rest x) "X." nil)

;; see: https://github.com/jwiegley/use-package
(defmacro hcRequire (name &rest body)
  "NAME BODY: poor man's 'use-package'."
  `(if (require ',name nil t)
       (progn ,@body)
     (warn (concat (format "%s" ',name) " NOT FOUND"))))

(hcRequire use-package)

;; https://github.com/emacs-mirror/emacs/commit/881be95cddcab3cf37373678002c35334c177c97
(setq package-review-policy t
      package-review-diff-command
      '("git" "diff" "--no-index" "--color=never" "--diff-filter=d"))

;; ------------------------------------------------------------------------------
(hcSection "Beans")
;; Never access a variable directly.

(defmacro hcDefineBean (name &rest body) "NAME BODY."
  (let ((var-name (intern (concat "*" (format "%s" name) "*"))))
    `(progn
       (defvar ,var-name ,@body)
       (defun ,name () ,var-name))))

;; ------------------------------------------------------------------------------
(hcSection "Predicates")

(defun hcIsVersionP    (x) "X." (string-match x (emacs-version)))
(defun hcXEmacsP       () "." (hcIsVersionP "XEmacs"))
(defun hcWin32P        () "." (or (equal window-system 'win32)
                                  (equal window-system 'w32)
                                  (equal window-system 'mswindows)
                                  (hcIsVersionP "cygwin")))
(defun hcDarwinP       () "." (hcIsVersionP "darwin"))
(defun hcLinuxP        () "." (hcIsVersionP "linux"))

(defun hcMachineName   () "." (replace-regexp-in-string
                               "\n$" ""
                               (shell-command-to-string "uname -n")))

;; ------------------------------------------------------------------------------
(hcSection "Key Bindings")

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html#Key-Bindings
;; http://www.emacswiki.org/emacs/KeyBindingDiscussion

;; the following swaps the default kill/copy
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-w" 'kill-ring-save)
;; default set-mark-command is \C-SPC
(global-set-key "\C-c\C-m" 'set-mark-command) ;; for ssh/vnc into hosted

;; when using vnc, "\C-y" is getting converted to a different key, so use this
(if (hcLinuxP) (global-set-key "\C-h" 'yank))

; C-x 5 o other-frame "frame.el"
; C-x o other-window "window.el"
(global-set-key "\C-x\C-o" 'other-frame) ; overwrites: delete-blank-lines "simple.el"
(global-set-key "\M-g" 'goto-line)

(autoload 'dabbrev "dabbrev" "dabbrev" t)
(if (not (hcWin32P))
  (global-set-key "\M-\ " 'dabbrev-expand)
  (global-set-key "\C-z"  'dabbrev-expand)) ; when all else fails

(global-set-key "\C-c\C-k" 'describe-char)

;; ------------------------------------------------------------------------------
(hcSection "Shell Commands")

(defun hcShExecCmd (name &rest args)
  "NAME ARGS: execute a shell command."
  (shell-command-to-string
   (concat (if (symbolp name) (symbol-name name) name)
           " "
	   (apply #'concat
		  (mapcar #'(lambda (arg) (format "%s " arg))
			  args)))))

(defmacro hcShDefCmd (name args)
  "NAME ARGS: create a function to execute a shell command."
  `(defun ,name ,args
     (apply #'hcShExecCmd (list ',name ,@args))))

(defmacro hcShDefCmdMemo (name)
  "NAME: create a memoized shell function."
  (let ((varName (intern (format "*%s*" name))))
    `(progn
       (defvar ,varName nil)
       (defun ,name ()
	 (cond (,varName)
	       (t (setq ,varName (hcShExecCmd ',name))))))))

;; ------------------------------------------------------------------------------
(hcSection "Locations")

;; Important (to me) directories.

(defun hcExpandFileName (forExternalProgramP path)
  "FOREXTERNALPROGRAMP PATH."
  (if (hcWin32P)
      (let ((result (shell-command-to-string (concat "cygpath " (if forExternalProgramP "-m " "-u " path)))))
        ;; Get rid of extra linefeed put in by shell-command-to-string.
        (substring result 0 (- (length result) 1)))
    (expand-file-name path)))

(defun hcLocation (name) "NAME." (hcShExecCmd 'hcLocation name))

(hcDefineBean hcEmacsDir     (hcLocation 'emacs))
(add-to-list 'load-path      (hcEmacsDir))

(hcDefineBean hcEsync        (hcLocation  'esync))
(hcDefineBean hcFinance      (hcLocation  'finance))
(hcDefineBean hcFsync        (hcLocation  'fsync))
(hcDefineBean hcFtptmp       (hcLocation  'ftptmp))
(hcDefineBean hcHome         (hcLocation  'home))
(hcDefineBean hcM2Repository (hcShExecCmd 'hcM2Repository))
(hcDefineBean hcMusic        (hcLocation  'music))
(hcDefineBean hcRpt          (hcLocation  'rpt))
(hcDefineBean hcSync         (hcLocation  'sync))
(hcDefineBean hcUlhcd        (hcLocation  'ulhcd))
(hcDefineBean hcWs           (hcLocation  'ws))

(hcShDefCmdMemo hcPathSep)
(hcShDefCmd     hcLibClasspath ())
(hcShDefCmd     hcFsToBs (string))
(hcShDefCmd     hcLlavaMainClass ())

;; ------------------------------------------------------------------------------
(hcSection "Top level misc stuff")

(advice-add 'risky-local-variable-p :override #'ignore)

;; https://emacsredux.com/blog/2026/04/04/read-extended-command-predicate/
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; https://www.emacswiki.org/emacs/GccEmacs#h5o-13
;; tell package.el to do ahead-of-time native compilation
;;
;; native-comp...
(setq package-native-compile t)
(defun hc-do-native-compile ()
  "."
  (interactive)
  (native-compile-async (concat (hcEmacsDir) "/.vanilla.emacs.d/elpa")  'recursively))

(use-package hc-buffer-control)

;; move/copy between two dired windows
(defvar dired-dwim-target)
(setq dired-dwim-target t)
(use-package hc-dired)

(setq tab-bar-show nil)

;; This must be ON for haskell-mode to work.
(use-package flycheck
             :config (global-flycheck-mode 1))
;; Ask before exit.
(setq confirm-kill-emacs
      (lambda (e)
        (y-or-n-p-with-timeout
         "Really exit Emacs (automatically exits in 5 secs)? " 5 t)))

;; don't ask when killing shell buffer (and other processes)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Do NOT use tabs for indenting
(setq-default indent-tabs-mode nil)

;; Why have extra do-nothing whitespace?
(setq-default show-trailing-whitespace  t)
(setq         indicate-empty-lines      t)

;; highlight text beyond nth column
(use-package whitespace
  :config
  (progn
    (setq whitespace-style '(face lines-tail))
    (setq whitespace-line-column 1000)
    (global-whitespace-mode t)))

;; so list-buffers won't jump back to top
(defvar global-auto-revert-non-file-buffers)
(setq global-auto-revert-non-file-buffers nil)

;; ** Display full filepath in title

;; http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; stop backup changing file creation date of original file
(setq backup-by-copying t)

(use-package grep
  :config
  ;; List of names of sub-directories which `rgrep' shall not recurse into.
  (setq grep-find-ignored-directories
        (cons "dist-newstyle"
        (cons "node_modules"
        (cons ".stack-work"
        (cons "target"
         grep-find-ignored-directories))))
  )
)

;; Useful in *Org Agenda* buffer.
(defun hc-line-spacing (n) "N. Put more space between lines or nil to regular."
  (interactive "nSize: ")
  (setq line-spacing n))

;; momentarily show the point of the window/buffer being switched to
(hcRequire pulse)
(defun pulse-line (&rest _)
  "Interactive function to pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))
(defadvice other-window        (after        other-window-pulse activate) "." (pulse-line))
(defadvice delete-window       (after       delete-window-pulse activate) "." (pulse-line))
(defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate) "." (pulse-line))
(defadvice other-frame         (after         other-frame-pulse activate) "." (pulse-line))
(setq pulse-delay 0.20)

;; TRAMP : do not ask to save passwords
(setq auth-source-save-behavior nil)

;; have 'M-x dictionary-search' use
(setq dictionary-server "dict.org")

;; ------------------------------------------------------------------------------
(hcSectionLoad hc-time)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-image)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-sync)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-pinboard-pick)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-browser-history)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-theme-switch)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-finance)

;; ------------------------------------------------------------------------------
;; * Markdown

(when (member (hcMachineName) hc-dev-machines)
  (hcSection "Markdown")
  (use-package hc-markdown))

;; ------------------------------------------------------------------------------
;; * PDF

(when (member (hcMachineName) hc-dev-machines)
  (hcSection "PDF")
  (use-package hc-pdf))

;; ------------------------------------------------------------------------------
(hcSectionLoad hc-gnus)

;; ------------------------------------------------------------------------------
;; * Registers and Bookmarks

;; - Registers
;;   - http://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html#Registers
;;   - http://emacswiki.org/emacs/Registers
;; - Bookmarks (like registers, but persistent)
;;   - http://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html#Bookmarks
;;   - http://emacswiki.org/emacs/BookMarks

;; TODO
;; - http://www.emacswiki.org/emacs-en/BookmarkPlus

(hcSection "Bookmarks")

(defvar bookmark-save-flag)
(setq   bookmark-save-flag 1)
(defvar bookmark-sort-flag)
(setq   bookmark-sort-flag nil)
(defvar bookmark-default-file)
(setq   bookmark-default-file (concat (hcEmacsDir) "/.emacs.bmk"))

;;(use-package hc-bookmark-plus)

;; ------------------------------------------------------------------------------
(hcSectionLoad hc-web-browsing)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-timestamp)

;; ------------------------------------------------------------------------------
;; * org-mode

(hcSection "OrgMode")
(with-no-warnings
(use-package org
  :defer t
  :config
  (progn (use-package hc-org-mode)
         (hc-org-mode)
         (when (member (hcMachineName) hc-dev-machines)
           (use-package hc-noter))))
)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-tags)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-git)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-sdedit)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-dir-tree)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-epub)

;; ------------------------------------------------------------------------------
;; * HEXL-MODE : view/edit at files like in "hex dump" format
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Binary-Files.html
;; ------------------------------------------------------------------------------
;;(hcSectionLoad hc-greek)

;; END   .emacs.common
;; ------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------
;; * named keyboard macros

(fset 'ecp
   (kmacro-lambda-form [?\C-s ?\[ ?l ?a ?b ?e ?l ?= ?\" ?E ?C ?P ?\C-e backspace backspace backspace ?\C-d ?\C-d ?\C-d] 0 "%d"))

;; ------------------------------------------------------------------------------
;; * Top level misc

(hcSection "Top level misc stuff")

(when (member (hcMachineName) hc-dev-machines)
  (desktop-save-mode 1))

;; Store customizations in a separate file.
(setq custom-file (concat (hcEmacsDir) "/.vanilla.emacs.custom.el"))
(load custom-file)

(load-file (concat (hcEmacsDir) "/hc-mode-line.el"))

;; WHICH KEY
(use-package which-key
  :ensure t
  :demand
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

;; do not minimize
(global-unset-key (kbd "C-z"))

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
(hcSectionLoad hc-completion)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-yasnippet)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-daemon-emacsclient)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-projectile)

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
(hcSection "Line Numbers")

;; http://www.emacswiki.org/LineNumbers
(use-package linum :defer t
;;  :config (setq global-linum-mode t) ;; always on
)

;; ------------------------------------------------------------------------------
;; * Agda
(when (member (hcMachineName) hc-dev-machines)
  (hcSectionLoad hc-agda))
;; ------------------------------------------------------------------------------
;; * Haskell
(when (member (hcMachineName) hc-dev-machines)
  (hcSectionLoad hc-haskell))
;; ------------------------------------------------------------------------------
;; * Python
;; (when (member (hcMachineName) hc-dev-machines)
;;   (hcSectionLoad hc-python))
;; ------------------------------------------------------------------------------
;; * Rust
(when (member (hcMachineName) hc-dev-machines)
  (hcSectionLoad hc-rust))
;; ------------------------------------------------------------------------------
;; * Typescript

(when (member (hcMachineName) hc-dev-machines)
  (hcSection "Typescript")
  (use-package typescript)
  (setq typescript-indent-level 2))

;; ------------------------------------------------------------------------------
(hcSectionLoad hc-java)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-lisps)

;; ------------------------------------------------------------------------------
;; This should come AFTER all language-specific setup above.
(when (member (hcMachineName) hc-dev-machines)
  (hcSection "LSP and EGLOT")
  (use-package hc-lsp-pick)
  (use-package hc-eglot)
  )

;; ------------------------------------------------------------------------------
;; * Images

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html

(hcSection "Images")

(defvar image-dired-dir)
(setq image-dired-dir "/tmp/emacs-image-dired/")

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

;; https://emacs.stackexchange.com/a/73152
(defun hc-reverse-dependencies (package-name)
  (let ((needle package-name)
        (resolve-deps-recursively t)
        curr-deps
        all-rdeps)
    (dolist (curr package-archive-contents all-rdeps)
      ;; (car curr) => pkg
      ;; (cadr curr) => pkg-desc
      (setq curr-deps (if nil ;; resolve-deps-recursively
                          (package--dependencies (car curr))
                        (mapcar #'car (package-desc-reqs (cadr curr)))))
      (when (memq needle curr-deps)
        (push (car curr) all-rdeps)))))

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

;; ------------------------------------------------------------------------------

;; XML/HTML
(defvar sgml-basic-offset)
(setq sgml-basic-offset 4)

;; ------------------------------------------------------------------------------
(hcSection "Open current buffer's associated file in an external program")

(use-package hc-run-command-package)

;; TODO : delete
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
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-font-size-frame-size)
;; ------------------------------------------------------------------------------
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

(defun hcColorZB () "."
   (interactive)
   (set-foreground-color "#DCDCCC")
   (set-background-color "#3F3F3F")
   (set-cursor-color     "#FFFFEF"))

(defun hcColorBW () "."
   (interactive)
   (set-foreground-color "#FFFFFF")
   (set-background-color "#000000")
   (set-cursor-color     "#FFFFFF"))

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

;; ------------------------------------------------------------------------------
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

;; ------------------------------------------------------------------------------
;; END STARTUP
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000000))

(provide '.vanilla.emacs)

;;; .vanilla.emacs ends here
