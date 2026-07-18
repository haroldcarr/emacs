;;; .vanilla.emacs --- init file          -*- lexical-binding: t; -*-

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

(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")
        ))

;; Prefer GNU ELPA over NonGNU ELPA over MELPA when versions are equal.
(setq package-archive-priorities
      '(("gnu"    . 30)
        ("nongnu" . 20)
        ("melpa"  . 10)
        ))

;; Pins for packages also built into Emacs,
;; e.g., eglot, jsonrpc, project, python, seq, and xref
;; enable newer GNU ELPA versions to override the bundled versions.
(setq package-pinned-packages
      '(
        (ace-window          . "gnu")
        (async               . "gnu")
        (avy                 . "gnu")
        (cape                . "gnu")
        (company             . "gnu")
        (compat              . "gnu")
        (cond-let            . "nongnu")
        (cl-generic          . "gnu")
        (cl-lib              . "gnu")
        (corfu               . "gnu")
        (dash                . "gnu")
        (diff-hl             . "gnu")
        (eat                 . "gnu")
        (edit-indirect       . "nongnu")
        (editorconfig        . "nongnu")
        (ef-themes           . "gnu")
        (eglot               . "gnu")
        (eldoc               . "gnu")
        (embark              . "gnu")
        (embark-consult      . "gnu")
        (emms                . "gnu")
        (erc                 . "gnu")
        (esxml               . "nongnu")
        (evil                . "nongnu")
        (external-completion . "gnu")
        (flycheck            . "nongnu")
        (flymake             . "gnu")
        (git-timemachine     . "melpa")
        (goto-chg            . "nongnu")
        (gptel               . "nongnu")
        (gptel-agent         . "melpa")
        ;; (haskell-mode     . "nongnu")   ; TODO: Uncomment once the compile bug is fixed.
        (helm                . "nongnu")
        (helm-core           . "nongnu")
        (hydra               . "gnu")
        (hyperbole           . "melpa")    ; TODO: switch to gnu once hywiki added to hyperbole/gnu
        (idlwave             . "gnu")
        (idris-mode          . "nongnu")
        (jinx                . "gnu")
        (js2-mode            . "gnu")
        (jsonrpc             . "gnu")
        (let-alist           . "gnu")
        (llama               . "nongnu")
        (lua-mode            . "nongnu")
        (lv                  . "gnu")
        (magit               . "nongnu")
        (magit-section       . "nongnu")
        (map                 . "gnu")
        (marginalia          . "gnu")
        (markdown-mode       . "nongnu")
        (memory-usage        . "gnu")
        (mmm-mode            . "gnu")
        (minimap             . "gnu")
        (modus-themes        . "gnu")
        (multiple-cursors    . "nongnu")
        (nadvice             . "gnu")
        (nano-theme          . "gnu")
        (nov                 . "gnu")
        (ntlm                . "gnu")
        (oauth2              . "gnu")
        (orderless           . "gnu")
        (peg                 . "gnu")
        (popup               . "nongnu")
        (posframe            . "gnu")
        (project             . "gnu")
        (projectile          . "nongnu")
        (prop-menu           . "nongnu")
        (python              . "gnu")
        (rainbow-mode        . "gnu")
        (request             . "nongnu")
        (seq                 . "gnu")
        (so-long             . "gnu")
        (soap-client         . "gnu")
        (spinner             . "gnu")
        (svg                 . "gnu")
        (tablist             . "nongnu")
        (track-changes       . "gnu")
        (tramp               . "gnu")
        (transient           . "gnu")
        (treepy              . "nongnu")
        (use-package         . "gnu")
        (vertico             . "gnu")
        (wfnames             . "nongnu")
        (which-key           . "gnu")
        (window-tool-bar     . "gnu")
        (with-editor         . "nongnu")
        (xref                . "gnu")
        (yaml                . "gnu")
        (yaml-mode           . "nongnu")
        (yasnippet           . "gnu")
        ))

;; 1. Restart Emacs.
;; 2. Run =M-x package-refresh-contents=.
;; 3. In =M-x list-packages=, install the pinned GNU/NonGNU versions.
;; 4. Mark obsolete versions with =~= and delete with =x=.
;; Keep the MELPA =haskell-mode= for now if NonGNU 17.5 still triggers the byte-compilation error.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(package-initialize)

(eval-and-compile
  (require 'cl-lib)
  (setq use-package-verbose t)
  (require 'use-package))

(require 'diminish nil t)

(add-to-list 'load-path (string-trim (shell-command-to-string "hcLocation emacs")))


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

(defmacro hcSectionLoad (name) "NAME."
  `(progn
     (hcSection ,(symbol-name name))
     (use-package ,name)))

(defmacro hcSectionLoadOnDevMachines (name) "NAME."
  `(when (member (hcMachineName) hc-dev-machines)
     (hcSectionLoad ,name)))

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
;;(global-set-key "\M-g" 'goto-line) ; use corfu M-g g

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

(setq initial-buffer-choice (concat (hcSync) "/DEIK/DEIK.org"))

;; Avoid Service Name Too Long error
(setq server-socket-dir "/tmp/hc-server-socket-dir")
(make-directory server-socket-dir t)

(advice-add 'risky-local-variable-p :override #'ignore)

;; https://emacsredux.com/blog/2026/04/04/read-extended-command-predicate/
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; https://www.emacswiki.org/emacs/GccEmacs#h5o-13
;; tell package.el to do ahead-of-time native compilation
;; native-comp...
(setq package-native-compile t)
(defun hc-do-native-compile ()
  "."
  (interactive)
  (native-compile-async (concat (hcEmacsDir) "/.vanilla.emacs.d/elpa")  'recursively))

(use-package hc-buffer-control)

(setq tab-bar-show nil)

;; This must be ON for haskell-mode to work.
(use-package flycheck :config (global-flycheck-mode 1))

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

;; stop backup changing file creation date of original file
(setq backup-by-copying t)

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

;; TRAMP : do not ask to save passwords
(setq auth-source-save-behavior nil)

;; have 'M-x dictionary-search' use
(setq dictionary-server "dict.org")

(when (member (hcMachineName) hc-dev-machines)
  (desktop-save-mode 1))

;; Store customizations in a separate file.
(setq custom-file (concat (hcEmacsDir) "/.vanilla.emacs.custom.el"))
(load custom-file)

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
(load-file (concat (hcEmacsDir) "/hc-mode-line.el"))
;;(load "time")
;;(display-time)
(line-number-mode)
(column-number-mode)

;; ** Display full filepath in title
;; http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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
(use-package pulse
  :config
  (setopt pulse-delay 0.20))

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(advice-add 'other-window        :after #'pulse-line)
(advice-add 'delete-window       :after #'pulse-line)
(advice-add 'recenter-top-bottom :after #'pulse-line)
(advice-add 'other-frame         :after #'pulse-line)

;; WHICH KEY
(use-package which-key
  :ensure t
  :demand
  :config
  (which-key-mode)
  ;; default 1.0
  (setopt which-key-idle-delay 1.0)
  ;; where popup shows      : side-window (default), minibuffer, frame, custom
  (setopt which-key-popup-type 'side-window)
  ;; where side window sits : bottom (default), top, left, right
  (setopt which-key-side-window-location 'bottom)
)

;; ** Make buffer names unique
;; Use part of the path name for buffer name when visiting two different files with same name.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html#Uniquify
;; http://emacswiki.org/emacs/uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator ":"))

(add-hook 'html-mode-hook  (lambda () (setq-local sgml-basic-offset 4)))
(add-hook 'mhtml-mode-hook (lambda () (setq-local sgml-basic-offset 4)))

;; ------------------------------------------------------------------------------
(hcSectionLoad hc-ai)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-spelling)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-time)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-music-emms)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-youtube)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-reaper)
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
(hcSectionLoadOnDevMachines hc-markdown)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-pdf)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-gnus)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-bookmarks)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-web-browsing)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-timestamp)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-org-mode)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-pkms) ;; must come after org-mode
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-tags)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-git)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-sdedit)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-dir-tree)
;; ------------------------------------------------------------------------------
;; move/copy between two dired windows
(defvar dired-dwim-target)
(setq dired-dwim-target t)
;;(use-package hc-dired)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-epub)
;; ------------------------------------------------------------------------------
;;(hcSectionLoad hc-greek)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-icr-mini-buffer-and-in-buffer-incremental-completing-read)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-yasnippet)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-daemon-emacsclient)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-projectile)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-compilation)
;; ------------------------------------------------------------------------------
(hcSection "Line Numbers")

;; http://www.emacswiki.org/LineNumbers
(use-package linum :defer t
;;  :config (setq global-linum-mode t) ;; always on
)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-agda)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-haskell)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-lean)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-lua)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-python)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-rust)
;; ------------------------------------------------------------------------------
;; * Typescript
;; (when (member (hcMachineName) hc-dev-machines)
;;   (hcSection "Typescript")
;;   (use-package typescript)
;;   (setq typescript-indent-level 2))
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-java)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-lisps)
;; ------------------------------------------------------------------------------
;; This should come AFTER all language-specific setup above.
(when (member (hcMachineName) hc-dev-machines)
  (hcSection "LSP and EGLOT")
  ;;(use-package hc-lsp-pick)
  (use-package hc-lsp)
  (use-package hc-eglot)
)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-calendar)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-alignment)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-google)
;; ------------------------------------------------------------------------------
;; * Misc

(hcSection "Misc")
(use-package httpcode :defer t)

(defun hc-tps (ntx ndays nhours nminutes nseconds)
  "NTX NDAYS NHOURS NMINUTES NSECONDS."
  (let ((days-seconds    (* (* (* ndays 24) 60) 60))
        (hours-seconds   (* (* nhours       60) 60))
        (minutes-seconds (* nminutes            60)))
    (/ ntx (+ days-seconds hours-seconds minutes-seconds nseconds))))

;; https://emacs.stackexchange.com/a/73152
(defun hc-reverse-dependencies (package-name)
  "PACKAGE-NAME."
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
(hcSectionLoad hc-appearance)
;; ------------------------------------------------------------------------------
;; END STARTUP
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000000))

(provide '.vanilla.emacs)

;;; .vanilla.emacs ends here
