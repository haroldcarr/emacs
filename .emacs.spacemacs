;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; ------------------------------------------------------------------------------
(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   ;; If symbol `all' instead of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Uncomment/add layer names
     ;; - press <SPC f e R> (Vim style) or
     ;; - <M-m f e R> (Emacs style)
     ;; to install them.
     ;; ----------------------------------------------------------------
     auto-completion   ;; i.e., company-mode
     ;; better-defaults
     emacs-lisp
     git
     haskell
     ;; markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     version-control
     hc-config
     )
   ;; packages that will be installed without being wrapped in a layer.
   ;; If configuration is needed, consider creating a layer.
   ;; Configuration can also be put in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(google-c-style)
   ;; packages/extensions that will not be installed/loaded.
   dotspacemacs-excluded-packages '(evil-escape)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

;; ------------------------------------------------------------------------------
(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(tsdh-dark
                         zenburn
                         monokai
                         solarized-light
                         solarized-dark
                         spacemacs-light
                         spacemacs-dark
                         leuven
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font.
   ;; `powerline-scale' : mode-line size to make separators look better.
   dotspacemacs-default-font '("Monaco"     ;; "Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

;; ------------------------------------------------------------------------------
(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; HC

  (defvar *hcSectionEnabled* nil)
  (defvar *hcSection* "")

  (defun hcSection (title)
    "For debugging .emacs"
    (cond (*hcSectionEnabled*
           (setq *hcSection* title)
           (message title))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Beans")

  (defmacro hcDefineBean (name &rest body)
    (let ((var-name (intern (concat "*" (format "%s" name) "*"))))
      `(progn
         (defvar ,var-name ,@body)
         (defun ,name () ,var-name))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Predicates")

  (defun hcIsVersionP    (x) (string-match x (emacs-version)))
  (defun hcUnameContains (x) (string-match x (shell-command-to-string "uname -a")))
  (defun hcOracleLinuxP  ()  (or (and (hcIsVersionP "redhat-linux")
                                      (hcIsVersionP "us.oracle.com"))
                                 (and (hcIsVersionP "x86_64-unknown-linux-gnu")
                                      (hcIsVersionP "2013-03-26 on adc2100420"))
                                 (and (hcUnameContains "Linux")
                                      (or (hcUnameContains "slcn19cn15ib")
                                          (hcUnameContains "slcn19cn16ib")
                                          (hcUnameContains "adc00phv")))))
  (defun hcDarwinP       ()  (hcIsVersionP "darwin"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Top level misc stuff")

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

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Key Bindings")

  ;; disable spacemacs escape treatment (make it like regular emacs)
  (define-key evil-emacs-state-map [escape] nil)
  ;; C-x 5 o other-frame   "frame.el"
  ;; C-x   o other-window "window.el"
  (global-set-key "\C-x\C-o" 'other-frame) ; overwrites: delete-blank-lines "simple.el"
  (global-set-key "\M-g" 'goto-line)
  ;; set-mark-command is \C-space

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Locations")

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "org-mode")

  (use-package org :config (hcOrgMode))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Timestamp")

  (load-library "hcTimestamp")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Bookmarks")

  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (concat (hcEmacsDir) "/.emacs.bmk"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Java")

  (defvar *hcJavaMode* 'google)
  (add-hook 'java-mode-hook
            (lambda () (if (eq *hcJavaMode* 'google) (google-set-c-style))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (hcSection "Appearance")

  (defun hc-h (n) (set-frame-height (selected-frame) n))
  (defun hc-w (n) (set-frame-width (selected-frame) n))

  (defun hcGreyBackground ()
    (interactive)
    (set-face-background 'default "grey")
    )

  (defun hcAppearance ()
    (interactive)
    (hcGreyBackground)
    ;; (hcRightScrollBar)
    )

  (defun hcMacFont ()
    (interactive)
    ;;(set-face-font 'default "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
    ;;(set-face-font 'default "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
    (set-face-font 'default "-apple-Monaco-medium-normal-normal-*-20-*-*-*-m-0-iso10646-1")
    ;;(set-frame-font "Source Code Pro-21" nil t)
    ;;(set-face-font 'default "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
    )

  (defun hcMacWidthHeight ()
    (interactive)
    (hc-w 100)
    (hc-h 27)
    )

  (defun hcMacFW ()
    (interactive)
    (hcMacWidthHeight)
    (hcMacFont)
    )

  (defun hcMacAppearance ()
    (interactive)
    (hcAppearance)
    (hcMacFont)
    (hcMacWidthHeight)
    )
  )

;; ------------------------------------------------------------------------------
(defun hcOrgMode ()
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|rpt\\|txt\\)$" . org-mode))
  ;; So I can visit script links in org files (instead of execute them)
  ;; I link to the real file: ln -s <some-script> <some-script>.hcScript
  (add-to-list 'org-file-apps '("\\.hcScript\\'" . emacs))

  ;; "Standard" key bindings (but not provided)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;;;
  ;;; Org Misc
  ;;;

  ;; If you do not like transient-mark-mode, you can create an active
  ;; region by using the mouse to select a region, or pressing C-<SPC>
  ;; twice before moving the cursor.
  (transient-mark-mode 1)

  ;;(setq org-hide-leading-stars t)

  ;; show the whole file when first visited
  (setq org-startup-folded nil)

  ;; don't fold when yanking
  (setq org-yank-folded-subtrees nil)

  ;; Org buffers only
  ;;(add-hook 'org-mode-hook 'turn-on-font-lock)

  ;; The default is 3
  (setq org-export-headline-levels 6)

  ;; Do not put the validate link at bottom of page
  (setq org-export-html-validation-link nil) ; I think this is obsolete.
  (setq org-html-postamble-format nil)       ; I think this is the replacement - does not seem to work.

  ;; Do not put timestamp at bottom of page
  (setq org-export-time-stamp-file nil)

  ;; Do not put author at bottom of page
  (setq org-export-author-info nil)

  ;; Do not put in validation link in HTML export
  (setq org-html-validation-link nil)

  ;; let ME control org-mode font colors, etc.
  (setq org-export-htmlize-output-type 'css)

  ;;;
  ;;; Agenda
  ;;;

  ;; Include entries from the emacs diary into =org-mode='s agenda.
  (setq org-agenda-include-diary t)

  ;; org-mode manages the =org-agenda-files= variable automatically using
  ;; C-c [ and C-c ] to add/remove files respectively.
  ;; Instead, disable those keys and replace with an explicit directory list.
  ;; Any org files in those directories are automatically included in the agenda.
  (setq org-agenda-files
        (list
         (concat (hcFsync)    "/TODO-ME.org")
         (concat (hcFinance)  "/01-TODO.org")
         (concat (hcRpt)      "/TODO-WORK.org")
         ;; (hcRpt)
         ;; (concat (hcRpt)   "/.past/2014")
         ;; (concat (hcRpt)   "/.past/2013")
         ;; (concat (hcRpt)   "/.past/2012")
         ;; (concat (hcRpt)   "/.past/2011")
         ;; "/tmp/google.org"
         ))

  (add-hook 'org-mode-hook
            (lambda ()
              (org-defkey org-mode-map "\C-c["    'undefined)
              (org-defkey org-mode-map "\C-c]"    'undefined)))

  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?E)
  (setq org-default-priority ?E)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)" "DELEGATED(D!/!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "SKIP" "PHONE")
          (sequence "OPEN(O!)" "|" "CLOSED(C!)")
          ))

  (setq org-todo-keyword-faces
        '(("TODO"       :foreground "yellow"         :weight bold)
          ("NEXT"       :foreground "blue"           :weight bold)
          ("STARTED"    :foreground "blue"           :weight bold)
          ("DONE"       :foreground "forest green"   :weight bold)
          ("DELEGATED"  :foreground "forest green"   :weight bold)

          ("WAITING"    :foreground "white"          :weight bold)
          ("SOMEDAY"    :foreground "orange"         :weight bold)
          ("CANCELLED"  :foreground "forest green"   :weight bold)
          ("SKIP"       :foreground "forest green"   :weight bold)
          ("PHONE"      :foreground "forest green"   :weight bold)

          ("OPEN"       :foreground "blue"           :weight bold)
          ("CLOSED"     :foreground "forest green"   :weight bold)
          ))

  ;;;
  ;;; Literate programming
  ;;;

  ;; Important: set this or it will remove space after editing code: C-c,C-c,'
  ;; The default is 2.
  (setq org-edit-src-content-indentation 4)

  ;; When exporting code I want it to look like what I wrote.
  (setq org-src-preserve-indentation t)

  ;; When editing code, use the current window.
  (setq org-src-window-setup (quote current-window))

  ;;; see http://doc.norang.ca/org-mode.html
  ;;; see http://home.fnal.gov/~neilsen/notebook/orgExamples/org-examples.html

  ;; http://ditaa.org/ditaa/
  ;; probably not needed since the jar comes with org-mode in contrib/scripts.
  (setq org-ditaa-jar-path    (concat (hcUlhcd) "/java/ditaa/ditaa0_9.jar"))

  ;; http://plantuml.sourceforge.net/
  (setq org-plantuml-jar-path (concat (hcUlhcd) "/java/plantuml/plantuml.7995.jar"))

  (add-hook 'org-babel-after-execute-hook 'hc/display-inline-images 'append)

  ;; Make babel results blocks lowercase
  (setq org-babel-results-keyword "results")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot        . t)
     (ditaa      . t)
     (gnuplot    . t)
     (haskell    . t)
     (latex      . t)
     (plantuml   . t)
     ))

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

  ;; Cache all babel results blocks by default
  ;; For graphics generation, this is faster if nothing changes
  (if (fboundp 'org-babel-default-header-args)
      (setq org-babel-default-header-args
            (cons '(:cache . "yes")
                  (assq-delete-all :cache org-babel-default-header-args))))
)

(defun hc/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
