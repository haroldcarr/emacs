;;; .emacs.common.el --- stuff used in vanilla and spacemacs           -*- mode: emacs-lisp -*-

;;; Commentary:

;; http://www.gnu.org/software/emacs/manual/index.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
;; http://emacswiki.org/

;;;;
;;;; Created       : a long time ago ...        by Harold Carr.
;;;; Last Modified : 2024 Sep 20 (Fri) 19:12:52 by Harold Carr.
;;;;

;;; Code:

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

(setq debug-on-error t)

(defmacro comment (&rest x) "X." nil)

;; see: https://github.com/jwiegley/use-package
(defmacro hcRequire (name &rest body)
  "NAME BODY: poor man's use-package."
  `(if (require ',name nil t)
       (progn ,@body)
     (warn (concat (format "%s" ',name) " NOT FOUND"))))

(hcRequire use-package)

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

;; SHELLS
;; eshell
;;   Implemented in Elisp. Integrated with Emacs. Runs on Windows.
;;   Does not support terminal manipulation capabilities (e.g., ncdu, nmtui, ...).
;; shell
;;    Uses standard shell (e.g., bash). reads input from Emacs, sends it to shell, displays shell output.
;;    Does not support interactive commands that handle how output should be displayed (e.g., htop).
;; term
;;    Terminal emulator written in Elisp. Runs a shell (like terminal emulator Gnome Terminal).
;;    Program output can manipulate output using escape codes. So htop will work
;;    But term and ansi-term do not implement all escapes codes.
;;    Some programs do not work properly. Has inferior performance.
;; vterm
;;    https://github.com/akermu/emacs-libvterm
;;    Terminal emulator. Core is written in C, libvterm.
;;    Fast and works with most terminal applications.

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

;; https://www.emacswiki.org/emacs/GccEmacs#h5o-13
;; tell package.el to do ahead-of-time native compilation
;;
;; native-comp...
(setq package-native-compile t)
(defun hc-do-native-compile ()
  "."
  (interactive)
  (native-compile-async (concat (hcEmacsDir) "/.vanilla.emacs.d/elpa")  'recursively))

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
        (cons ".stack-work"
        (cons "target"
         grep-find-ignored-directories)))
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

;;(when (hcIsVersionP "28")
;;  (setc tab-bar-format '(tab-bar-format-global)
;;        tab-bar-mode t))

;; (defun hc-split-window-sensibly (&optional window)
;;   "WINDOW.  Replace `split-window-sensibly' with one that prefers vertical splits."
;;   (interactive)
;;   (if (and (fboundp 'window-in-direction)
;;                        ;; Don't try to split when starting in a minibuffer
;;                        ;; e.g M-: and try to use helm-show-kill-ring.
;;                        (not (minibufferp helm-current-buffer)))
;;       (let ((window (or window (selected-window))))
;;         ;; (or (and (window-splittable-p window t)
;;         ;;          (with-selected-window window
;;         ;;            (split-window-right)))
;;         ;;     (and (window-splittable-p window)
;;         ;;          (with-selected-window window
;;         ;;            (split-window-below)))))))
;;         (if (window-splittable-p window)
;;             (with-selected-window window (split-window-below))))))

;; (setq      split-window-preferred-function-ORIG split-window-preferred-function)
;; (setq      split-window-preferred-function #'hc-split-window-sensibly)
;; (setq helm-split-window-preferred-function #'hc-split-window-sensibly)


;; ------------------------------------------------------------------------------
;; * Time

(hcSection "Time")

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(use-package time
  :custom
  (world-clock-list
   '(("Pacific/Honolulu"     "Honolulu")
     ("America/Los_Angeles"  "San Francisco")
     ("America/Denver"       "Salt Lake City")
     ("America/Chicago"      "Austin")
     ("America/New_York"     "New York")
     ("America/Santiago"     "Santiago, Chile")
     ("Europe/London"        "London")
     ("Europe/Amsterdam"     "Amsterdam")
     ("Europe/Paris"         "Paris")
     ;;("Asia/Calcutta"        "Bangalore")
     ;;("Asia/Tokyo"           "Tokyo")
     ("Pacific/Auckland"     "Wellington, New Zealand")
     )))

;; ------------------------------------------------------------------------------
;; * Images

(hcSection "Images")

(use-package hc-image)

;; ------------------------------------------------------------------------------
;; * Sync

(hcSection "Sync")
(use-package hc-sync)

;; ------------------------------------------------------------------------------
;; * Pinboard

(hcSection "Pinboard")
(use-package hc-pinboard)

;; ------------------------------------------------------------------------------
;; * ChatGPT

(hcSection "ChatGPT")
(use-package hc-chat-gpt)

;; ------------------------------------------------------------------------------
;; * Browser History

(hcSection "Browser History")
(use-package hc-browser-history)

;; ------------------------------------------------------------------------------
;; * Themes

(hcSection "Themes")
(use-package hc-theme-switch)

;; ------------------------------------------------------------------------------
;; * Markdown

(when (member (hcMachineName) '("o2023" "o2020" "o2015"))
  (hcSection "Markdown")
  (use-package hc-markdown))

;; ------------------------------------------------------------------------------
;; * PDF

(when (member (hcMachineName) '("o2023" "o2020" "o2015"))
  (hcSection "PDF")
  (use-package hc-pdf))

;; ------------------------------------------------------------------------------
;; * GNUS

(hcSection "GNUS")

;; https://whereofwecannotspeak.wordpress.com/2009/07/15/getting-gnus-to-read-mail-over-imap/

;; Get an app-specific password: https://support.google.com/accounts/answer/185833

;;;; RECEIVE
;; https://lars.ingebrigtsen.no/2020/01/06/whatever-happened-to-news-gmane-org/comment-page-1/#comment-36418
(defvar gnus-select-method)
(setq gnus-select-method
      '(nntp "news.gmane.io")
      )
(comment
(defvar gnus-secondary-select-methods)
(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-authenticator login)
                (nnimap-expunge-on-close 'never)
                (nnimap-stream ssl))))
)
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
(setq   bookmark-save-flag 1)
(defvar bookmark-sort-flag)
(setq   bookmark-sort-flag nil)
(defvar bookmark-default-file)
(setq   bookmark-default-file (concat (hcEmacsDir) "/.emacs.bmk"))

;;(use-package hc-bookmark-plus)

;; ------------------------------------------------------------------------------
;; * Web Browsing

(hcSection "Web Browsing")
(use-package hc-web-browsing)

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
(use-package hc-timestamp)
)
;; ------------------------------------------------------------------------------
;; * org-mode

(hcSection "OrgMode")
(with-no-warnings
(use-package org
  :defer t
  :config
  (progn (use-package hc-org-mode)
         (hc-org-mode)
         (when (member (hcMachineName) '("o2023" "o2020"))
           (use-package hc-noter))))
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

(hcSection "Version Control and Magit")

(hcSection "git")
(with-no-warnings
(use-package hc-git)
)

;; I am not sure, but I think after I set these to nil, emacs started leaving *~ files around
;; even though the editted files were under source control.
;;(setopt vc-follow-symlinks nil)
;;(setopt vc-handled-backends nil)

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

(defun hc-h (n)    "N."   (interactive) (set-frame-height (selected-frame) n))
(defun hc-w (n)    "N."   (interactive) (set-frame-width (selected-frame) n))
(defun hc-hw (x y) "X Y." (interactive) (hc-h x) (hc-w y))
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

(defun hc-font-size (n) "N."
  (interactive "nSize: ")
  (hcFonts (* n 10) (* n 15)))

;; ------------------------------------------------------------------------------
;; * directory tree

(hcSection "directory tree")
(load-library "hc-dir-tree.el")

;; ------------------------------------------------------------------------------
;; * EPUB

(hcSection "EPUB")

;; https://github.com/wasamasa/nov.el
(with-no-warnings
(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
)

;; ------------------------------------------------------------------------------
;; * HEXL-MODE : view/edit at files like in "hex dump" format

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Binary-Files.html


(defun hc-signed-off-by ()
  "X."
  (interactive)
  (insert "Signed-off-by: Harold Carr harold.carr@oracle.com"))

;; ------------------------------------------------------------------------------
;; * greek

(progn
  (global-set-key (kbd "M-] a") "α")
  (global-set-key (kbd "M-] b") "β")
  (global-set-key (kbd "M-] g") "γ")
  (global-set-key (kbd "M-] d") "δ")
  (global-set-key (kbd "M-] e") "ε")
  (global-set-key (kbd "M-] z") "ζ")
  (global-set-key (kbd "M-] h") "η")
  (global-set-key (kbd "M-] q") "θ")
  (global-set-key (kbd "M-] i") "ι")
  (global-set-key (kbd "M-] k") "κ")
  (global-set-key (kbd "M-] l") "λ")
  (global-set-key (kbd "M-] m") "μ")
  (global-set-key (kbd "M-] n") "ν")
  (global-set-key (kbd "M-] x") "ξ")
  (global-set-key (kbd "M-] o") "ο")
  (global-set-key (kbd "M-] p") "π")
  (global-set-key (kbd "M-] r") "ρ")
  (global-set-key (kbd "M-] s") "σ")
  (global-set-key (kbd "M-] t") "τ")
  (global-set-key (kbd "M-] u") "υ")
  (global-set-key (kbd "M-] f") "ϕ")
  (global-set-key (kbd "M-] j") "φ")
  (global-set-key (kbd "M-] c") "χ")
  (global-set-key (kbd "M-] y") "ψ")
  (global-set-key (kbd "M-] w") "ω")
  (global-set-key (kbd "M-] A") "Α")
  (global-set-key (kbd "M-] B") "Β")
  (global-set-key (kbd "M-] G") "Γ")
  (global-set-key (kbd "M-] D") "Δ")
  (global-set-key (kbd "M-] E") "Ε")
  (global-set-key (kbd "M-] Z") "Ζ")
  (global-set-key (kbd "M-] H") "Η")
  (global-set-key (kbd "M-] Q") "Θ")
  (global-set-key (kbd "M-] I") "Ι")
  (global-set-key (kbd "M-] K") "Κ")
  (global-set-key (kbd "M-] L") "Λ")
  (global-set-key (kbd "M-] M") "Μ")
  (global-set-key (kbd "M-] N") "Ν")
  (global-set-key (kbd "M-] X") "Ξ")
  (global-set-key (kbd "M-] O") "Ο")
  (global-set-key (kbd "M-] P") "Π")
  (global-set-key (kbd "M-] R") "Ρ")
  (global-set-key (kbd "M-] S") "Σ")
  (global-set-key (kbd "M-] T") "Τ")
  (global-set-key (kbd "M-] U") "Υ")
  (global-set-key (kbd "M-] F") "Φ")
  (global-set-key (kbd "M-] J") "Φ")
  (global-set-key (kbd "M-] C") "Χ")
  (global-set-key (kbd "M-] Y") "Ψ")
  (global-set-key (kbd "M-] W") "Ω")
)

(provide '.emacs.common)

;;; .emacs.common.el ends here
