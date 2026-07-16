;;; hc-icr-mb-action-embark.el --- hc-icr-mb-action-embark          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------------------------------------------------------
;; EMBARK : The Action Layer
;; https://github.com/oantolin/embark

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  ;;(setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t) ; only need to install it, embark loads it after consult if found

(provide 'hc-icr-mb-action-embark)


;; use:
;; C-. O   ;; reveal in Finder

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "O")
    (lambda (f)
      (interactive "fFile: ")
      (call-process "open" nil 0 nil "-R" (expand-file-name f)))))

;; see current bindings
;; M-x describe-keymap RET embark-file-map

;; (defun hc-embark-remove-defun-target-in-org ()
;;   (setq-local embark-target-finders
;;               (cl-remove #'embark-target-defun-at-point
;;                          embark-target-finders)))

;; (add-hook 'org-mode-hook #'hc-embark-remove-defun-target-in-org)
