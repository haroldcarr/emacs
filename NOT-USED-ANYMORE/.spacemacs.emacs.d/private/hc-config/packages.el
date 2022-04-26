;;; packages.el --- hc-config Layer packages File for Spacemacs

;;; Commentary:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

;;; Code:

(defvar hc-config-packages)
(setq hc-config-packages
      '(
        ;; JAVA via eclim + eclipse
        eclim
        company-emacs-eclim

        ;; HASKELL
        haskell-snippets
        hindent
        hlint-refactor
        intero

        ;; MISC
        markdown-preview-mode
        peep-dired

        ;; THEMES
        autumn-light-theme
        ))

;; List of packages to exclude.
(defvar hc-config-excluded-packages)
(setq hc-config-excluded-packages '())

;; -----------------------------------------------------------------------------
;;
;; MANUALLY START ECLIPSE
;;
;; http://eclim.org/install.html
;; - Download ECLIPSE NEON 64 BIT (full app - not installer)
;; - mkdir -p /Volumes/HC/.sync/ftptmp/ECLIPSE/workspace
;; - LAUNCH
;;   - Workspace : /Volumes/HC/.sync/ftptmp/ECLIPSE/workspace
;; ECLIM
;; - graphic install : java -jar ~/Downloads/eclim_2.6.0.jar

;; USAGE: https://github.com/emacs-eclim/emacs-eclim/wiki
;;
;; COMPLETION
;;     M-`         / company-emacs-eclim
;;         automatic completion as you type (requires some typing to kick in)
;;             use key binding to show all possibilities before you type
;;     M-TAB       / eclim-complete
;;         displays buffer of completions
;; LIST PROBLEMS
;;     C-c C-e b   / eclim-problems
;;         display errors/warnings
;;             switch between errors/warnings via "e" and "w"
;;             jump to source via RET
;;             "g" to refresh
;;     eclim-problems-compilation-buffer
;;         compilation buffer with errors and warnings
;;             move around via next-error (C-`) and prev-error
;; FIND
;;     M-.         / eclim-java-find-declaration
;;         go to definition of identifier under point
;;     C-c C-e f t / eclim-java-find-type
;;          go to class definition by entering name
;;     C-c C-e f r / eclim-java-find-references
;;          display references to identifier under point
;; IMPORTS
;;     C-c C-e i   / eclim-java-import-organize
;;          adds imports
;;              Preferences -> Java -> Code Style -> Organize Imports
;;                  catalog-service/etc/ides/eclipse-google-style.importorder
;; CLASS HIERARCHY :
;;     C-c C-e h   / eclim-java-hierarchy
;;          display hierarchy tree for class in the current buffer
(defvar eclim-mode-map)
(defvar help-at-pt-timer-delay)
(defun hc-config/init-eclim ()
  (use-package eclim
    :config
    (progn
      (global-eclim-mode)
      (custom-set-variables
       '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
       '(eclim-executable     "/Applications/Eclipse.app/Contents/Eclipse/eclim"))

      (define-key eclim-mode-map (kbd "M-.")
        (lambda ()
          (interactive)
          (eclim-java-find-declaration)
          (recenter-top-bottom)))

      (define-key eclim-mode-map (kbd "M-`") 'company-emacs-eclim)

      ;; https://www.bountysource.com/issues/43113986-shouldn-t-problems-appear-in-the-minibuffer-when-you-move-your-cursor-over-them
      (setq help-at-pt-display-when-idle t)
      (setq help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      ;; Ulises eclim bindings
      ;;(define-key eclim-mode-map (kbd "C-c C-e C-f") 'eclim-problems-correct)
      ;;(define-key eclim-mode-map (kbd "C-c C-e C-p C-n") 'eclim-problems-next-same-window)
      ;;(define-key eclim-mode-map (kbd "C-c C-e C-p C-p") 'eclim-problems-previous-same-window)

      ;; Ulises helm-eclim bindings
      ;;(define-key eclim-mode-map (kbd "C-c C-e h") nil)
      ;;(define-key eclim-mode-map (kbd "C-c C-e h t") 'helm-eclim-java-find-type)
      ;;(define-key eclim-mode-map (kbd "C-c C-e h u") 'helm-eclim--update-cache)
      ;;(define-key eclim-mode-map (kbd "C-c C-e h m") 'helm-eclim-java-find-method)
      ;;(define-key eclim-mode-map (kbd "C-c C-e h r") 'helm-eclim-java-find-references)
      )))

;; code completion: https://github.com/emacs-eclim/emacs-eclim/wiki/Code-Completion
(defvar company-emacs-eclim)
(defun hc-config/init-company-emacs-eclim () "."
  (use-package company-emacs-eclim
    :config
    (progn
      (company-emacs-eclim-setup)
      (global-company-mode t))))

;; ------------------------------------------------------------------------------
(defvar haskell-snippets)
(defun hc-config/init-haskell-snippets () "."
  (use-package haskell-snippets))

(defun hc-config/init-hindent () "."
  (use-package hc-haskell-format))

(defvar hlint-refactor)
(defun hc-config/init-hlint-refactor () "."
  (use-package hlint-refactor))

(defvar flycheck-check-syntax-automatically)
(defun hc-config/init-intero () "."
  (use-package intero
    :config (progn (add-hook 'haskell-mode-hook 'intero-mode)
                   ;; https://github.com/commercialhaskell/intero/issues/208
                   (setq flycheck-check-syntax-automatically '(mode-enabled save)))))

;; ------------------------------------------------------------------------------
(defvar markdown-preview-mode)
(defun hc-config/init-markdown-preview-mode () "."
  (use-package markdown-preview-mode
    :ensure t
    :defer t
    :init
    (add-hook 'js-mode-hook #'indium-interaction-mode)
  ))

;; ------------------------------------------------------------------------------
(defvar peep-dired)
(defun hc-config/init-peep-dired ()
  "LINK: http://pragmaticemacs.com/emacs/quickly-preview-images-and-other-files-with-peep-dired/ ."
  ;;preview files in dired
  (use-package peep-dired
    :ensure t
    :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
                ("P" . peep-dired))
    ))

;; ------------------------------------------------------------------------------

(defun hc-config/init-autumn-light-theme () "."
  (use-package autumn-light-theme
    :ensure t
    :defer t
    ))

(provide 'packages)
;;; packages.el ends here

