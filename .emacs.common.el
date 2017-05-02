;;; .emacs.common.el --- stuff used in vanilla and spacemacs

;;; Commentary:

;; http://www.gnu.org/software/emacs/manual/index.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
;; http://emacswiki.org/

;;;;
;;;; Created       : a long time ago ...        by Harold Carr.
;;;; Last Modified : 2017 May 01 (Mon) 17:40:33 by Harold Carr.
;;;;

;;; Code:

;; ------------------------------------------------------------------------------
;; * Sections

;; Name each "section" in this .emacs file.
;;
;; Then, when an error occurs, you can tell what section it occurs in
;; by looking in the *Messages* buffer or examining the *hcSection* variable.
;;
;; =debug-on-error= is finer grained, but this is still useful.

(defvar *hcSectionEnabled* t)
(defvar *hcSection* "")

(defun hcSection (title)
  "For debugging .emacs. TITLE."
  (cond (*hcSectionEnabled*
         (setq *hcSection* title)
         (message title))))

(setq debug-on-error nil)

(defmacro comment (&rest x) "X." nil)

;; see: https://github.com/jwiegley/use-package
(defmacro hcRequire (name &rest body)
  "NAME BODY: poor man's use-package."
  `(if (require ',name nil t)
       (progn ,@body)
     (warn (concat (format "%s" ',name) " NOT FOUND"))))

(hcRequire use-package)

;; ------------------------------------------------------------------------------
;; * Beans

;; Never access a variable directly.

(hcSection "Beans")

(defmacro hcDefineBean (name &rest body) "NAME BODY."
  (let ((var-name (intern (concat "*" (format "%s" name) "*"))))
    `(progn
       (defvar ,var-name ,@body)
       (defun ,name () ,var-name))))

;; ------------------------------------------------------------------------------
;; * Predicates

(hcSection "Predicates")

(defun hcIsVersionP    (x) "X." (string-match x (emacs-version)))
(defun hcUnameContains (x) "X." (string-match x (shell-command-to-string "uname -a")))


(defun hcLucidP        () "." (hcIsVersionP "Lucid"))
(defun hcXEmacsP       () "." (hcIsVersionP "XEmacs"))
(defun hcXP            () "." (equal window-system 'x))
(defun hcOracleLinuxP  () "." (or (and (hcIsVersionP "redhat-linux")
                                       (hcIsVersionP "us.oracle.com"))
                                  (and (hcIsVersionP "x86_64-unknown-linux-gnu")
                                       (hcIsVersionP "2013-03-26 on adc2100420"))
                                  (and (hcUnameContains "Linux")
                                       (or (hcUnameContains "slcn19cn15ib")
                                           (hcUnameContains "slcn19cn16ib")
                                           (hcUnameContains "adc00phv")))))
(defun hcWin32P        () "." (or (equal window-system 'win32)
                                  (equal window-system 'w32)
                                  (equal window-system 'mswindows)
                                  (hcIsVersionP "cygwin")))
(defun hcDarwinP       () "." (hcIsVersionP "darwin"))

;; ------------------------------------------------------------------------------
;; * Key Bindings

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html#Key-Bindings
;; http://www.emacswiki.org/emacs/KeyBindingDiscussion

(hcSection "Key Bindings")

;; NOTE: set-mark-command is \C-space
;; the following swaps the default kill/copy
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-w" 'kill-ring-save)

; C-x 5 o other-frame "frame.el"
; C-x o other-window "window.el"
(global-set-key "\C-x\C-o" 'other-frame) ; overwrites: delete-blank-lines "simple.el"
(global-set-key "\M-g" 'goto-line)

(autoload 'dabbrev "dabbrev" "dabbrev" t)
(if (not (hcWin32P))
  (global-set-key "\M-\ " 'dabbrev-expand)
  (global-set-key "\C-z"  'dabbrev-expand)) ; when all else fails

;; ------------------------------------------------------------------------------
;; * Executing shell commands

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
  "NAME ARGS: create a function that execuates a shell command."
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
;; * Locations

;; Important (to me) directories.

(hcSection "Locations")

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
(hcDefineBean hcRpt          (hcLocation  'rpt))
(hcDefineBean hcSync         (hcLocation  'sync))
(hcDefineBean hcUlhcd        (hcLocation  'ulhcd))
(hcDefineBean hcWs           (hcLocation  'ws))

(hcShDefCmdMemo hcPathSep)
(hcShDefCmd     hcLibClasspath ())
(hcShDefCmd     hcFsToBs (string))
(hcShDefCmd     hcLlavaMainClass ())

;; ------------------------------------------------------------------------------
;; * Top level misc

(hcSection "Top level misc stuff")

;; This must be ON for haskell-mode to work.
(with-no-warnings
(use-package flycheck
             :config (global-flycheck-mode 1))
)
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
(with-no-warnings
(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-line-column 100)
  (global-whitespace-mode t))
)
;; so list-buffers won't jump back to top
(defvar global-auto-revert-non-file-buffers)
(setq global-auto-revert-non-file-buffers nil)

;; ** Display full filepath in title

;; http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; ------------------------------------------------------------------------------
;; * GNUS

;; https://whereofwecannotspeak.wordpress.com/2009/07/15/getting-gnus-to-read-mail-over-imap/

;; Get an app-specific password: https://support.google.com/accounts/answer/185833

;;;; RECEIVE
(defvar gnus-select-method)
(setq gnus-select-method
      '(nntp "news.gmane.org"))

(defvar gnus-secondary-select-methods)
(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-authenticator login)
                (nnimap-expunge-on-close 'never)
                (nnimap-stream ssl))))

;; Original value was  "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
(defvar gnus-summary-line-format)
(defvar gnus-user-date-format-alist)
(setq gnus-summary-line-format "%&user-date;%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d")))

;;;; SEND
(defvar message-send-mail-function)
(defvar smtpmail-starttls-credentials)
(defvar smtpmail-auth-credentials)
(defvar smtpmail-default-smtp-server)
(defvar smtpmail-smtp-server)
(defvar smtpmail-smtp-service)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "harold.carr@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mail-host-address "harold.carr@gmail.com") ;; gets rid of "tickle me"

;; M-x gnus

;; when it prompts for your password, give app-specific password
;; -  (and optionally let it store that password ---unprotected--- in =~/.authinfo=)

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
(defvar bookmark-default-file)
(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat (hcEmacsDir) "/.emacs.bmk"))

;; ------------------------------------------------------------------------------
;; * Timestamp

;; - ftp://202.5.194.21/SW_ebooks/EMAGAZINE/Writing_GNU_Emacs_Extensions.pdf
;;   - starting on page 47

(hcSection "Timestamp")

;; when running on samsun nc10; cygwin; startxwin; emacs; this is defined with
;; the computer name and my name and it screws up - so eval this by hand
;; TODO : it doesn't get redefined below because hcWin32P is false
;; because the window-system is x
;;(defun user-full-name () "Harold Carr")
(with-no-warnings
(use-package hcTimestamp)
)
;; ------------------------------------------------------------------------------
;; * org-mode

(hcSection "OrgMode")
(with-no-warnings
(use-package org
  :defer t
  :config
  (progn (use-package hcInitOrgMode)
         (hcOrgMode)))
)
;; ------------------------------------------------------------------------------
;; * Tags

;; - http://www.emacswiki.org/emacs/BuildTags
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Tags.html#Tags
;; - http://emacswiki.org/emacs/EmacsTags

(hcSection "Tags")

;; alternate way to create using etags
;; cd <...>
;; need a regex instead of "*" - also only files
;; find . -name "*" -print -o -name SCCS -name RCS -prune | .../bin/etags -

(defun hcTagsCreate (dir-name &optional tags-dir-path-filename)
  "DIR-NAME TAGS-DIR-PATH-FILENAME: Create tags file."
  (interactive "DDirectory: ")
  ;; ctags via nix
  (let* ((dir (directory-file-name dir-name))
         (ctags-filename (if (null tags-dir-path-filename) (concat dir "/TAGS") tags-dir-path-filename)))
    (shell-command
     (format "ctags -f %s -e -R %s" ctags-filename dir))))

(defun        hcTagsDir       (x)"X."(concat (hcEsync)        "/TAGS/" x))
(hcDefineBean hcTagsCatalogSrc       (concat (hcWs)           "/catalog-service/subprojects/catalog-core/src/"))
(hcDefineBean hcTagsCatalogDst       (hcTagsDir               "TAGS-CATALOG"))
(hcDefineBean hcTagsJavaSrc          "/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/src")
(hcDefineBean hcTagsJavaDst          (hcTagsDir               "TAGS-JAVA"))
(hcDefineBean hcTagsBuzzSrc          (concat (hcWs)           "/buzz-message-bus/src"))
(hcDefineBean hcTagsBuzzDst          (hcTagsDir               "TAGS-BUZZ"))
(hcDefineBean hcTagsMessageBusSrc    (concat (hcM2Repository) "/com/oracle/commons/fmw-commons/12.1.4-0-0-SNAPSHOT/sources/src"))
(hcDefineBean hcTagsMessageBusDst    (hcTagsDir               "TAGS-MESSAGE-BUS"))
(defun hcTagsCreateCatalog    ()"."(interactive) (hcTagsCreate (hcTagsCatalogSrc)    (hcTagsCatalogDst)))
(defun hcTagsCreateJava       ()"."(interactive) (hcTagsCreate (hcTagsJavaSrc)       (hcTagsJavaDst)))
(defun hcTagsCreateBuzz       ()"."(interactive) (hcTagsCreate (hcTagsBuzzSrc)       (hcTagsBuzzDst)))
(defun hcTagsCreateMessageBus ()"."(interactive) (hcTagsCreate (hcTagsMessageBusSrc) (hcTagsMessageBusDst)))
(defun hcTagsCreateAll () "."
  (interactive)
  (hcTagsCreateCatalog)
  (hcTagsCreateJava)
  (hcTagsCreateBuzz)
  (hcTagsCreateMessageBus))
(defun hcVtc ()"."(interactive) (visit-tags-table (hcTagsCatalogDst)))
(defun hcVtj ()"."(interactive) (visit-tags-table (hcTagsJavaDst)))
(defun hcVtb ()"."(interactive) (visit-tags-table (hcTagsBuzzDst)))
(defun hcVtm ()"."(interactive) (visit-tags-table (hcTagsMessageBusDst)))

;; ------------------------------------------------------------------------------
;; * Version Control and Magit

;; Version Control
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html#Version-Control
;; - http://emacswiki.org/emacs/VersionControl
;; GIT
;; - http://magit.github.com/magit/magit.html
;; - http://www.emacswiki.org/emacs/Magit
;; - https://github.com/pidu/git-timemachine
;; Magit and Ediff
;; - http://dachary.org/?p=2893

(hcSection "Version Control and Magit")

(hcSection "git")
(with-no-warnings
(if (not (fboundp 'spacemacs-mode))
    (use-package magit
     :config
     (progn
       (setq git-commit-summary-max-length 80)
       (setq git-commit-fill-column        80)
       )))
)
;; ------------------------------------------------------------------------------
;; * Clojure/Cider

;; http://jr0cket.co.uk/2015/09/spacemacs-for-clojure-development-configure-clojure.html

(hcSection "Clojure")

(declare-function cider-current-connection "")
(declare-function cider-repl-return "")
(declare-function cider-last-sexp "")
;; modified from cider-interaction.el
(defun hc-cider-insert-in-repl (form)
  "Insert FORM in the REPL buffer and switch to it.
If EVAL is non-nil the form will also be evaluated."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-connection)
    (goto-char (point-max))
    (let ((beg (point)))
      (insert form)
      (indent-region beg (point)))
    (cider-repl-return)))

(defun hc-cider-insert-last-sexp-in-repl (&optional arg)
  "Insert the expression preceding point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (hc-cider-insert-in-repl (cider-last-sexp)))

;; ------------------------------------------------------------------------------
;; * Send diagram text to SDEDIT (UML sequence diagrams)

;; http://sdedit.sourceforge.net/

;; When the current buffer contains SDEDIT diagram text, just do
;; M-x sdedit

;; Be sure the sdedit program is up and running as a service.

(hcSection "SDEDIT")

(defun hcSdedit () "."
  (interactive)
  (let ((p (open-network-stream "*HC-SDEDIT*" "*HC-SDEDIT-CONNECTION*" "localhost" "60001")))
    (process-send-string p (concat (buffer-name) "
" (buffer-string)))
    (delete-process p)))

;; ------------------------------------------------------------------------------
;; * frame/font size

(hcSection "frame/font size")

;; C-U C-X : shows current font

;; C-x C-- : decrease font size
;; C-x C-+ : increase font size
;; C-x C-0 : reset to default size
;; these run text-scale-adjust

(defun hc-h (n)    "N."   (set-frame-height (selected-frame) n))
(defun hc-w (n)    "N."   (set-frame-width (selected-frame) n))
(defun hc-hw (x y) "X Y." (hc-h x) (hc-w y))
(defun hc-hwd ()   "."    (interactive) (hc-h 27) (hc-w 101))

(defun hcFonts (default-height variable-pitch-height)
  "DEFAULT-HEIGHT VARIABLE-PITCH-HEIGHT."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Monaco"
                      :height variable-pitch-height
                      :weight 'regular))

(defun hcDoFonts () "."
  (interactive)
  (when window-system
    (cond ((> (x-display-pixel-width) 1800)
           (hcFonts 200 300))
          (t (hcFonts 175 200)))))

;; ------------------------------------------------------------------------------
;; * neotree
(with-no-warnings
(use-package neotree
  :config (progn (setq neo-theme 'arrow)
                 (setq neo-window-width 25)
                 (setq neo-buffer--show-hidden-file-p t)))
)

(provide '.emacs.common)

;;; .emacs.common.el ends here
