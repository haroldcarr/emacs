;;; hc-haskell-init --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2017 Oct 19 (Thu) 10:59:07 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

(defvar hc-emacs-location (shell-command-to-string "hcLocation emacs"))

(defun hc-load (filename) "FILENAME."
  (interactive)
  (load-file (concat hc-emacs-location "/" filename)))

;; https://github.com/cydparser/demo-emacs-haskell/blob/master/demo.org

(use-package haskell-mode
  :ensure t
  :defer t
  ;;:bind (:map haskell-mode-map
  ;;            ("M-g i" . haskell-navigate-imports)
  ;;            ("M-g M-i" . haskell-navigate-imports))
  ;;:init
  ;;(progn
  ;;  (setq haskell-compile-cabal-build-alt-command
  ;;        "cd %s && stack clean && stack build --ghc-options -ferror-spans"
  ;;        haskell-compile-cabal-build-command
  ;;        "cd %s && stack build --ghc-options -ferror-spans"
  ;;        haskell-compile-command
  ;;        "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s"))
)

(use-package haskell-snippets
  :ensure t
  :defer t)

;;(use-package hlint-refactor
;;  :ensure t
;;  :defer t
;;  :diminish ""
;;  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

;;(hc-load "hc-haskell-intero.el")
(hc-load "hc-haskell-dante.el")

(provide 'hc-haskell-init)

;;; hc-haskell-init.el ends here

