;;; Initialize package

;;; Utilities

(defun init-kill-buffer-current ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;; Global Configuration

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

;; Delete the trailing newline.
(setq kill-whole-line t)

;; Adjust indentation and line wrapping.
(let ((spaces 2)
      (max-line-length 100))
  (setq-default fill-column max-line-length
                indent-tabs-mode nil
                tab-width spaces
                tab-stop-list (number-sequence spaces max-line-length spaces)))

(bind-key "C-c C-SPC" #'delete-trailing-whitespace)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'init-kill-buffer-current)
(bind-key "M-o" #'other-window)

(global-subword-mode 1)

;;; General Packages

(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))


(use-package yaml-mode
  :defer t)

;;; Demo Packages

(use-package demo-it
  :defer t)

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package fancy-narrow
  :defer t)

(use-package org
  :defer t
  :init
  (progn
    (setq org-hide-emphasis-markers t
          org-log-done 'time
          org-src-fontify-natively t
          org-startup-truncated nil))
  :config
  (progn
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (sh . t))))))

(use-package org-bullets
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook #'org-bullets-mode)))

(use-package org-tree-slide
  :defer t)

(use-package zenburn-theme
  :demand
  :init
  (progn
    ;; Increase contrast for presentation.
    (defvar zenburn-override-colors-alist
      '(("zenburn-bg-1"     . "#101010")
        ("zenburn-bg-05"    . "#202020")
        ("zenburn-bg"       . "#2B2B2B")
        ("zenburn-bg+05"    . "#383838")
        ("zenburn-bg+1"     . "#3F3F3F")
        ("zenburn-bg+2"     . "#494949")
        ("zenburn-bg+3"     . "#4F4F4F")))
    (load-theme 'zenburn 'no-confirm)))

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
  :bind (("M-." . init-goto-tag))
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


;;; Enable Disabled Features

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
