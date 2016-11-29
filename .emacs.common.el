;;; | ====================================================== |
;;; | DO NOT EDIT the generated *.el file.                   |
;;; | It was generated from an org-mode "literate" version.  |
;;; | ====================================================== |

;;;;
;;;; Created       : a long time ago ...        by Harold Carr.
;;;; Last Modified : 2016 Nov 28 (Mon) 18:25:01 by Harold Carr.
;;;;

(defvar *hcSectionEnabled* t)
(defvar *hcSection* "")

(defun hcSection (title)
  "For debugging .emacs"
  (cond (*hcSectionEnabled*
         (setq *hcSection* title)
         (message title))))

(setq debug-on-error nil)

(defmacro comment (&rest x) nil)

;; see: https://github.com/jwiegley/use-package
(defmacro hcRequire (name &rest body)
  `(if (require ',name nil t)
       (progn ,@body)
     (warn (concat (format "%s" ',name) " NOT FOUND"))))

(hcRequire use-package)

(hcSection "Beans")

(defmacro hcDefineBean (name &rest body)
  (let ((var-name (intern (concat "*" (format "%s" name) "*"))))
    `(progn
       (defvar ,var-name ,@body)
       (defun ,name () ,var-name))))

(hcSection "Predicates")

(defun hcIsVersionP    (x)(string-match x (emacs-version)))
(defun hcUnameContains (x)(string-match x (shell-command-to-string "uname -a")))


(defun hcLucidP        () (hcIsVersionP "Lucid"))
(defun hcXEmacsP       () (hcIsVersionP "XEmacs"))
(defun hcXP            () (equal window-system 'x))
(defun hcOracleLinuxP  () (or (and (hcIsVersionP "redhat-linux")
                                   (hcIsVersionP "us.oracle.com"))
                              (and (hcIsVersionP "x86_64-unknown-linux-gnu")
                                   (hcIsVersionP "2013-03-26 on adc2100420"))
                              (and (hcUnameContains "Linux")
                                   (or (hcUnameContains "slcn19cn15ib")
                                       (hcUnameContains "slcn19cn16ib")
                                       (hcUnameContains "adc00phv")))))
(defun hcWin32P        () (or (equal window-system 'win32)
			      (equal window-system 'w32)
			      (equal window-system 'mswindows)
                              (hcIsVersionP "cygwin")))
(defun hcDarwinP       () (hcIsVersionP "darwin"))

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

(hcSection "Shell Commands")

(defun hcShExecCmd (name &rest args)
  (shell-command-to-string
   (concat (if (symbolp name) (symbol-name name) name)
           " "
	   (apply #'concat
		  (mapcar #'(lambda (arg) (format "%s " arg))
			  args)))))

(defmacro hcShDefCmd (name args)
  `(defun ,name ,args
     (apply #'hcShExecCmd (list ',name ,@args))))

(defmacro hcShDefCmdMemo (name)
  (let ((varName (intern (format "*%s*" name))))
    `(progn
       (defvar ,varName nil)
       (defun ,name ()
	 (cond (,varName)
	       (t (setq ,varName (hcShExecCmd ',name))))))))

(hcSection "Locations")

(defun hcExpandFileName (forExternalProgramP path)
  (if (hcWin32P)
      (let ((result (shell-command-to-string (concat "cygpath " (if forExternalProgramP "-m " "-u " path)))))
        ;; Get rid of extra linefeed put in by shell-command-to-string.
        (substring result 0 (- (length result) 1)))
    (expand-file-name path)))

(defun hcLocation (name) (hcShExecCmd 'hcLocation name))

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

(hcSection "Top level misc stuff")

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
(setq-default show-trailing-whitespace     t)
(setq         default-indicate-empty-lines t)

;; highlight text beyond nth column
(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-line-column 100)
  (global-whitespace-mode t))

;; so list-buffers won't jump back to top
(setq global-auto-revert-non-file-buffers nil)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;;; RECEIVE
(setq gnus-select-method
      '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-authenticator login)
                (nnimap-expunge-on-close 'never)
                (nnimap-stream ssl))))

;; Original value was  "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
(setq gnus-summary-line-format "%&user-date;%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d")))

;;;; SEND
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "harold.carr@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mail-host-address "harold.carr@gmail.com") ;; gets rid of "tickle me"

(hcSection "Bookmarks")

(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat (hcEmacsDir) "/.emacs.bmk"))

(hcSection "Timestamp")

; when running on samsun nc10; cygwin; startxwin; emacs; this is defined with
; the computer name and my name and it screws up - so eval this by hand
; TODO : it doesn't get redefined below because hcWin32P is false
; because the window-system is x
;(defun user-full-name () "Harold Carr")
(use-package hcTimestamp)

(hcSection "OrgMode")

(use-package org
  :defer t
  :config
  (progn (use-package hcInitOrgMode)
         (hcOrgMode)))

(hcSection "Tags")

; alternate way to create using etags
; cd <...>
; need a regex instead of "*" - also only files
; find . -name "*" -print -o -name SCCS -name RCS -prune | .../bin/etags -

(defun hcTagsCreate (dir-name &optional tags-dir-path-filename)
  "Create tags file."
  (interactive "DDirectory: ")
  ;; ctags via nix
  (let* ((dir (directory-file-name dir-name))
         (ctags-filename (if (null tags-dir-path-filename) (concat dir "/TAGS") tags-dir-path-filename)))
    (shell-command
     (format "ctags -f %s -e -R %s" ctags-filename dir))))

(defun        hcTagsDir       (x)    (concat (hcEsync)        "/TAGS/" x))
(hcDefineBean hcTagsCatalogSrc       (concat (hcWs)           "/catalog-service/subprojects/catalog-core/src/"))
(hcDefineBean hcTagsCatalogDst       (hcTagsDir               "TAGS-CATALOG"))
(hcDefineBean hcTagsJavaSrc          "/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/src")
(hcDefineBean hcTagsJavaDst          (hcTagsDir               "TAGS-JAVA"))
(hcDefineBean hcTagsBuzzSrc          (concat (hcWs)           "/buzz-message-bus/src"))
(hcDefineBean hcTagsBuzzDst          (hcTagsDir               "TAGS-BUZZ"))
(hcDefineBean hcTagsMessageBusSrc    (concat (hcM2Repository) "/com/oracle/commons/fmw-commons/12.1.4-0-0-SNAPSHOT/sources/src"))
(hcDefineBean hcTagsMessageBusDst    (hcTagsDir               "TAGS-MESSAGE-BUS"))
(defun hcTagsCreateCatalog    () (interactive) (hcTagsCreate (hcTagsCatalogSrc)    (hcTagsCatalogDst)))
(defun hcTagsCreateJava       () (interactive) (hcTagsCreate (hcTagsJavaSrc)       (hcTagsJavaDst)))
(defun hcTagsCreateBuzz       () (interactive) (hcTagsCreate (hcTagsBuzzSrc)       (hcTagsBuzzDst)))
(defun hcTagsCreateMessageBus () (interactive) (hcTagsCreate (hcTagsMessageBusSrc) (hcTagsMessageBusDst)))
(defun hcTagsCreateAll ()
  (interactive)
  (hcTagsCreateCatalog)
  (hcTagsCreateJava)
  (hcTagsCreateBuzz)
  (hcTagsCreateMessageBus))
(defun hcVtc () (interactive) (visit-tags-table (hcTagsCatalogDst)))
(defun hcVtj () (interactive) (visit-tags-table (hcTagsJavaDst)))
(defun hcVtb () (interactive) (visit-tags-table (hcTagsBuzzDst)))
(defun hcVtm () (interactive) (visit-tags-table (hcTagsMessageBusDst)))

(hcSection "Version Control and Magit")

(hcSection "git")
(if (not (fboundp 'spacemacs-mode))
    (use-package magit
     :config
     (progn
       (setq git-commit-summary-max-length 80)
       (setq git-commit-fill-column        80)
       )))

  (hcSection "Clojure")

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

(hcSection "SDEDIT")

(defun hcSdedit ()
  (interactive)
  (let ((p (open-network-stream "*HC-SDEDIT*" "*HC-SDEDIT-CONNECTION*" "localhost" "60001")))
    (process-send-string p (concat (buffer-name) "
" (buffer-string)))
    (delete-process p)))

(hcSection "frame/font size")

;; C-U C-X : shows current font

;; C-x C-- : decrease font size
;; C-x C-+ : increase font size
;; C-x C-0 : reset to default size
;; these run text-scale-adjust

(defun hc-h (n) (set-frame-height (selected-frame) n))
(defun hc-w (n) (set-frame-width (selected-frame) n))
(defun hc-hw (x y) (hc-h x) (hc-w y))
(defun hc-hwd () (interactive) (hc-h 27) (hc-w 101))

(provide '.emacs.common)

;;; End of file.
