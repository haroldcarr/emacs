;;; init.el --- haskell

;;; Initialize package

;;; Commentary:

;;; Code:

(defvar hc-emacs "hceh")

(defvar hc-emacs-location (shell-command-to-string "hcLocation emacs"))
(add-to-list 'load-path hc-emacs-location)

(defun hc-load (filename) "FILENAME."
  (interactive)
  (load-file (concat hc-emacs-location "/" filename)))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(defun hcMacFW () "."
  (interactive)
  (set-frame-width (selected-frame)  88) ;; 100
  (set-frame-height (selected-frame) 23) ;;  27                 20
  (if (string-match "darwin" (emacs-version))
      (set-face-font 'default "-apple-Monaco-medium-normal-normal-*-24-*-*-*-m-0-iso10646-1"))
  )
(hcMacFW)

;; move/copy between two dired windows
(defvar dired-dwim-target)
(setq dired-dwim-target t)

;; NOTE: set-mark-command is \C-space
;; the following swaps the default kill/copy
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-w" 'kill-ring-save)

; C-x 5 o other-frame "frame.el"
; C-x o other-window "window.el"
(global-set-key "\C-x\C-o" 'other-frame) ; overwrites: delete-blank-lines "simple.el"
(global-set-key "\M-g" 'goto-line)

(defvar bookmark-save-flag)
(defvar bookmark-default-file)
(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat hc-emacs-location "/.emacs.bmk"))

(setq-default show-trailing-whitespace  t)
(setq         indicate-empty-lines      t)

;; stop backup changing file creation date of original file
(setq backup-by-copying t)

;; ==============================================================================

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(setq package-archive-priorities '(("melpa" . 10)
                                   ("gnu"   .  5)
                                  ))
(setq package-enable-at-startup nil)
(package-initialize)

;;; Initialize use-package

(defvar use-package-always-ensure)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;; ------------------------------------------------------------------------------
;;; HC

(hc-load "hc-mode-line.el")
(hc-load "hc-neotree.el")

;; ------------------------------------------------------------------------------
;;; Utilities

(defun init-kill-buffer-current ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;; Global Configuration

;; Store customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Store auto-saves and backups in emacs.d/var.
(let* ((vdir (expand-file-name "var" user-emacs-directory))
       (adir (expand-file-name "autosaves/" vdir))
       (ldir (expand-file-name "auto-save-list/" vdir))
       (bdir (expand-file-name "backups/" vdir)))
  (make-directory adir t)
  (make-directory bdir t)
  (setq auto-save-file-name-transforms `((".*" ,(concat adir "\\1") t))
        auto-save-list-file-prefix (concat ldir "/saves-")
        backup-directory-alist `((".*" . ,bdir))))

(when (member "Inconsolata" (font-family-list))
  (set-frame-font "Inconsolata 15"))

;; Simplify prompts.
(fset 'yes-or-no-p 'y-or-n-p)

;; Reduce noise.
(setq confirm-nonexistent-file-or-buffer nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      ring-bell-function 'ignore)

;; Prevent accidental closure.
(setq confirm-kill-emacs 'y-or-n-p)

;; Display column number in modeline.
(setq column-number-mode t)

;; Collect garbage less frequently.
(setq gc-cons-threshold 104857600)

;; Adjust indentation and line wrapping.
(let ((spaces 2)
      (max-line-length 100))
  (setq-default fill-column max-line-length
                indent-tabs-mode nil
                tab-width spaces
                tab-stop-list (number-sequence spaces max-line-length spaces)))

;; Open URLs within Emacs.
(when (package-installed-p 'eww)
  (setq browse-url-browser-function 'eww-browse-url))

;; HC
(defun hc-browse (&optional arg)
  "ARG: From browse-url.el."
  (interactive "P")
  (let ((url (browse-url-url-at-point)))
    (if url
        (eww-browse-with-external-browser url)
      (error "No URL found"))))

(bind-key "C-c C-SPC" #'delete-trailing-whitespace)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'init-kill-buffer-current)
(bind-key "M-o" #'other-window)

(global-subword-mode 1)

;; ------------------------------------------------------------------------------
;;; General Packages

(hc-load "hc-company.el")

(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(hc-load "hc-helm.el")

(use-package which-key
  :demand
  :pin melpa
  :init (which-key-mode))

(use-package yaml-mode
  :defer t)

(hc-load "hc-yasnippet.el")

;; ------------------------------------------------------------------------------
;;; Development Packages

(use-package compile
  :defer t
  :init
  (progn
    (setq compilation-scroll-output 'first-error)

    (defun init-compilation-colorize ()
      "Colorize compilation."
      (let ((inhibit-read-only t))
        (goto-char compilation-filter-start)
        (move-beginning-of-line nil)
        (ansi-color-apply-on-region (point) (point-max))))

    (add-hook 'compilation-filter-hook #'init-compilation-colorize)))

(use-package etags
  ;;HC :bind (("M-." . init-goto-tag))
  :init
  (progn
    (setq tags-revert-without-query t))
  :config
  (progn
    (defun init-goto-tag ()
      "Jump to the definition."
      (interactive)
      (find-tag (find-tag-default)))))

(use-package flycheck
  :demand
  :diminish ""
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode))
  :config
  (progn
    (defun init-flycheck-may-enable-mode (f)
      "Disallow flycheck in special buffers."
      (interactive)
      (and (not (string-prefix-p "*" (buffer-name)))
           (apply (list f))))

    (advice-add 'flycheck-may-enable-mode :around
                #'init-flycheck-may-enable-mode)))

(use-package magit
  :defer t
  :init
  (progn
    (setq magit-push-always-verify nil
          magit-revert-buffers t)
    (add-hook 'git-commit-mode-hook #'flyspell-mode)))

(use-package paren
  :defer t
  :init
  (show-paren-mode))

(hc-load "hc-projectile.el")

;;; Haskell Packages

(hc-load "hc-haskell.el")

;; ------------------------------------------------------------------------------
;;; Enable Disabled Features

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ------------------------------------------------------------------------------
;;; HC

;; show the whole file when first visited
(setq org-startup-folded nil)

(provide 'init)

;;; init.el ends here
