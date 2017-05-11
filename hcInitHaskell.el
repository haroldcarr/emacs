;;; hcInitHaskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2017 May 07 (Sun) 15:38:52 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

;; https://github.com/cydparser/demo-emacs-haskell/blob/master/demo.org

(use-package haskell-mode
  :ensure t
  :defer t
;;HC  :bind (:map haskell-mode-map
;;              ("M-g i" . haskell-navigate-imports)
;;              ("M-g M-i" . haskell-navigate-imports))
  :init
  (progn
    (setq haskell-compile-cabal-build-alt-command
          "cd %s && stack clean && stack build --ghc-options -ferror-spans"
          haskell-compile-cabal-build-command
          "cd %s && stack build --ghc-options -ferror-spans"
          haskell-compile-command
          "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s")))

(use-package haskell-snippets
  :ensure t
  :defer t)

(use-package hlint-refactor
  :ensure t
  :defer t
  :diminish ""
  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(use-package intero
  :ensure t
  :defer t
  :diminish " λ"
  :bind (:map intero-mode-map
              ("M-." . init-intero-goto-definition))
  :init
  (progn
    (defun init-intero ()
      "Enable Intero unless visiting a cached dependency."
      (if (and buffer-file-name
               (string-match ".+/\\.\\(stack\\|stack-work\\)/.+" buffer-file-name))
          (progn
            (eldoc-mode -1)
            (flycheck-mode -1))
        (intero-mode)
        (setq projectile-tags-command "codex update")))

    (add-hook 'haskell-mode-hook #'init-intero))
  :config
  (progn
    (defun init-intero-goto-definition ()
      "Jump to the definition of the thing at point using Intero or etags."
      (interactive)
      (or (intero-goto-definition)
          (find-tag (find-tag-default))))

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))

;; ------------------------------------------------------------------------------
;; HC

(defvar hc-haskell-format-on-save t)

(defvar haskell-stylish-on-save)
(defvar hindent-reformat-buffer-on-save)

(defun hcToggleHaskellFormatOnSave () "."
  (interactive)
  (setq hc-haskell-format-on-save (not hc-haskell-format-on-save))
  (custom-set-variables
   '(haskell-stylish-on-save         hc-haskell-format-on-save)
   '(hindent-reformat-buffer-on-save nil))
  (message "haskell-stylish-on-save: %s; hindent-reformat-buffer-on-save: %s"
           haskell-stylish-on-save      hindent-reformat-buffer-on-save))

(use-package hindent
  :ensure t
  :config (progn (add-hook 'haskell-mode-hook 'hindent-mode)
                 (custom-set-variables
                  '(hindent-indent-size 4)
                  '(hindent-line-length 120)
                  '(hindent-style "johan-tibell")
                  '(haskell-stylish-on-save hc-haskell-format-on-save)
                  '(hindent-reformat-buffer-on-save nil))))

(provide 'hcInitHaskell)

;;; hcInitHaskell.el ends here

