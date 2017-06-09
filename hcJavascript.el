;;; hcJavascript.el --- javascript development environment

;;; Commentary:
;;;
;;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
;;; https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html

;;; Code:

(defvar js2-mode)
(defvar js-mode-map)
(use-package js2-mode
  ;;:ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  ))

;; refactoring: https://github.com/magnars/js2-refactor.el/blob/master/README.md
(defvar js2-refactor)
(defvar js2-mode-map)
(use-package js2-refactor
  ;;:ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
    (define-key js-mode-map (kbd "M-.") nil)
  ))

(defvar xref-js2)
(use-package xref-js2
  ;;:ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
    (define-key esc-map "." #'xref-find-definitions)
    ))

(defvar company)
(use-package company
  ;;:ensure t
  )
(defvar company-tern)
(defvar company-backends)
(defvar tern-mode-keymap)
(use-package company-tern
  ;;:ensure t
  :config
  (progn
    (add-to-list 'company-backends 'company-tern)
    (add-hook 'js2-mode-hook
              (lambda ()
                (tern-mode)
                (company-mode)))
    (define-key tern-mode-keymap (kbd "M-.") nil)
    (define-key tern-mode-keymap (kbd "M-,") nil)
    ))

;; ------------------------------------------------------------------------------

;; https://github.com/NicolasPetton/Indium
;; https://gitter.im/indium-emacs/Lobby

(use-package indium
  ;;:ensure t
  )

;; ------------------------------------------------------------------------------
(provide 'hcJavascript)

;;; hcJavascript.el ends here