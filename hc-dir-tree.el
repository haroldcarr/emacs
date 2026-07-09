1;;; hc-dir-tree.el --- Summry dir-tree

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treemacs
;; https://github.com/Alexander-Miller/treemacs/tree/d6acbaf81abf719c58d577a3a5bc18010fa85fbd

(eval-when-compile (require 'use-package))

(use-package treemacs
  :ensure t
  :defer t
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ;; BEGIN HC
    (setq treemacs-git-mode          nil
          treemacs-filewatch-mode    nil
          treemacs-python-executable nil)
    ;; END HC
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-hide-dot-jj-directory           t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ;; ("M-0"       . treemacs-select-window)
        ;; ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ;; ("C-x t d"   . treemacs-select-directory)
        ;; ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag)
        ))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; HC: buggy : see commented out advice-add (that did not work) below.
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;;(treemacs-start-on-boot)

;; if consult-preview-key is turned on for bookmarks, it sometimes gets errors:
;;
;; 1 -> (treemacs-icons-dired--display-icons-for-subdir "/Users/carr/.sync/.fsync/finance/" #<marker at 1 in finance>)
;; 1 <- treemacs-icons-dired--display-icons-for-subdir: !non-local\ exit!
;;
;; I instrumented the function in the trace with  (message "FILE=%S ICON=%S" file icon) ;; HC
;; FILE="/Users/carr/ftptmp/.DS_Store" ICON=nil
;;
;; The following uses the fallback for all cases where these is no icon.
;; But these did NOT work.
;; (with-eval-after-load 'treemacs
;;   (advice-add
;;    'treemacs-icon-for-file
;;    :filter-return
;;    (lambda (icon)
;;      (message "treemacs-icon-for-file %S" icon)
;;      (or icon (treemacs-get-icon-value 'fallback)))))

;; (with-eval-after-load 'treemacs
;;   (advice-add
;;    'treemacs-icon-for-dir
;;    :filter-return
;;    (lambda (icon)
;;      (message "treemacs-icon-for-dir %S" icon)
;;      (or icon (treemacs-get-icon-value 'fallback)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree

(use-package neotree
  :bind (:map neotree-mode-map
              ("a"     . neotree-stretch-toggle)
              ("TAB"   . neotree-stretch-toggle)
              ("C-x 1" . neotree-stretch-toggle)
              )
  :config (progn (setq neo-theme 'arrow)
                 (setq neo-window-width 30)
                 (setq neo-show-hidden-files t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired-sidebar
;; https://github.com/jojojames/dired-sidebar

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (cond
   ((eq system-type 'darwin)
    (if (display-graphic-p)
        (setq dired-sidebar-theme 'icons)
      (setq dired-sidebar-theme 'nerd))
    ;;(setq dired-sidebar-face '(:family "Helvetica" :height 140))
    )
   ((eq system-type 'windows-nt)
    (setq dired-sidebar-theme 'nerd)
    ;;(setq dired-sidebar-face '(:family "Lucida Sans Unicode" :height 110))
    )
   (:default
    (setq dired-sidebar-theme 'nerd)
    ;;(setq dired-sidebar-face '(:family "Arial" :height 140))
   ))

  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/knpatel401/filetree

;; M-x filetree-show ...

(use-package filetree
  :ensure nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hc-dir-tree)

;;; hc-dir-tree.el ends here
