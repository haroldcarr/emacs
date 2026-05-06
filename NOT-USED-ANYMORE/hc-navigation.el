;;; hc-navigation --- navigate -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;;(defmacro helm-aif (test then &optional else)
;;  `(let ((it ,test))
;;     (if it ,then ,else)))

;; SYNC WITH HELM-PROJECTILE BELOW IF THIS CHANGES
(defvar helm-M-x-fuzzy-match)
(defvar helm-apropos-fuzzy-match)
(defvar helm-buffers-fuzzy-matching)
(defvar helm-ff-newfile-prompt-p)
(defvar helm-locate-fuzzy-match)
(defvar helm-recentf-fuzzy-match)

(use-package helm
  :ensure nil
  :demand
  :diminish ""
  :bind (("C-M-y"     . helm-show-kill-ring)
        ;("C-h a"     . helm-apropos)
         ("C-x C-f"   . helm-find-files)
         ("C-x b"     . helm-mini)
         ("M-s o"     . helm-occur)
         ("M-x"       . helm-M-x)
         ("C-c p s a" . helm-projectile-ack)
         :map helm-map
         ([tab]       . helm-execute-persistent-action))
  :init
  (progn
    (setq helm-M-x-fuzzy-match           t
          helm-apropos-fuzzy-match       t
          helm-buffers-fuzzy-matching    t
          helm-ff-newfile-prompt-p       nil
          helm-locate-fuzzy-match        t
          helm-recentf-fuzzy-match       t
          helm-M-x-use-completion-styles nil)
    ;;(require 'helm-config)
    (helm-mode)))

;; SYNC WITH HELM ABOVE IF THIS CHANGES
(use-package helm-projectile
  :ensure nil
  :after projectile
  :demand
  :init
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

;; ------------------------------------------------------------------------------

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

;; ** Recently visited files

;; TODO
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/

(provide 'hc-navigation)

;;; hc-navigation.el ends here

