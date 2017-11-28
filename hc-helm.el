;;; hc-helm -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; SYNC WITH HELM-PROJECTILE BELOW IF THIS CHANGES
(defvar helm-M-x-fuzzy-match)
(defvar helm-apropos-fuzzy-match)
(defvar helm-buffers-fuzzy-matching)
(defvar helm-ff-newfile-prompt-p)
(defvar helm-locate-fuzzy-match)
(defvar helm-recentf-fuzzy-match)

(use-package helm
  :ensure t
  :demand
  :diminish ""
  :bind (("C-M-y" . helm-show-kill-ring)
        ;("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-s o" . helm-occur)
         ("M-x" . helm-M-x)
         :map helm-map
         ([tab] . helm-execute-persistent-action))
  :init
  (progn
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-ff-newfile-prompt-p nil
          helm-locate-fuzzy-match t
          helm-recentf-fuzzy-match t)
    (require 'helm-config)
    (helm-mode)))

;; SYNC WITH HELM ABOVE IF THIS CHANGES
(use-package helm-projectile
  :ensure t
  :after projectile
  :demand
  :init
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(provide 'hc-helm)

;;; hc-helm.el ends here
