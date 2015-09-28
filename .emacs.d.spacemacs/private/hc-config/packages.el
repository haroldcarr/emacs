;;; packages.el --- hc-config Layer packages File for Spacemacs

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq hc-config-packages
      '(emacs-eclim gud java-mode))

;; List of packages to exclude.
(setq hc-config-excluded-packages '())

(defun hc-config/init-emacs-eclim ()
  "eclim"
  (use-package eclim
    :config
    (progn
      (use-package eclimd)       ;; starts/stop clim from emacs
      (use-package eclim-debug)  ;; debug integration with gdb/gud

      (global-eclim-mode)
      (custom-set-variables
       '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse/"))
       '(eclim-executable     "/Applications/Eclipse.app/Contents/Eclipse/eclim"))
      (setq help-at-pt-display-when-idle t)
      (setq help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      ;; Ulises eclim bindings
      (define-key eclim-mode-map (kbd "C-c C-e p b") 'eclim-project-build)
      (define-key eclim-mode-map (kbd "C-c C-e C-f") 'eclim-problems-correct)
      (define-key eclim-mode-map (kbd "C-c C-e C-p C-n") 'eclim-problems-next-same-window)
      (define-key eclim-mode-map (kbd "C-c C-e C-p C-p") 'eclim-problems-previous-same-window)
      ;; helm-eclim bindings
      (define-key eclim-mode-map (kbd "C-c C-e h") nil)
      (define-key eclim-mode-map (kbd "C-c C-e h t") 'helm-eclim-java-find-type)
      (define-key eclim-mode-map (kbd "C-c C-e h u") 'helm-eclim--update-cache)
      (define-key eclim-mode-map (kbd "C-c C-e h m") 'helm-eclim-java-find-method)
      (define-key eclim-mode-map (kbd "C-c C-e h r") 'helm-eclim-java-find-references)
      (define-key eclim-mode-map (kbd "M-.") (lambda ()
                                               (interactive)
                                               (eclim-java-find-declaration)
                                               (recenter-top-bottom)))
      (define-key eclim-mode-map (kbd "M-,") 'pop-global-mark)

      ;; paredit for java
      (add-hook 'java-mode-hook 'my-config-paredit-nonlisp)
      (add-hook 'java-mode-hook
                (lambda () (setq eclim-project-name (eclim-project-name))))

      ;; company stuff
      (use-package company)
      (use-package company-emacs-eclim)
      (company-emacs-eclim-setup)
      (global-company-mode t)

      ;; gud-mode stuff
      (define-key eclim-mode-map (kbd "C-c C-a C-b") 'gud-break)
      (define-key eclim-mode-map (kbd "C-c C-a C-n") 'gud-next)
      (define-key eclim-mode-map (kbd "C-c C-a C-s") 'gud-step)
      (define-key eclim-mode-map (kbd "C-c C-a C-c") 'gud-cont)
      (define-key eclim-mode-map (kbd "C-c C-a C-r") 'gud-run)

      ;; misc stuff
      ;;(global-set-key (kbd "C-s") 'helm-occur)

      ;; Haskell
      ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
      ;;(setq haskell-interactive-popup-errors nil)
      ;;(setq haskell-indent-spaces 2)
      ;;(setq haskell-process-args-ghci "ghci")
      ;;(setq haskell-process-path-ghci "stack")
      ;;(setq haskell-process-type 'stack-ghci)
      )))

(defun hc-config/init-java-mode ()
  (defvar *hcJavaMode* 'google)
  (add-hook 'java-mode-hook
            (lambda () (if (eq *hcJavaMode* 'google) (google-set-c-style)))))

;; For each package, define a function hc-config/init-<package-name>
;;
;; (defun hc-config/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
