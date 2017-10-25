;;; hc-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2017 Oct 24 (Tue) 18:02:22 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

(defvar hc-emacs-location (shell-command-to-string "hcLocation emacs"))

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

;;(use-package hc-haskell-intero.el)
(use-package hc-haskell-dante)

(provide 'hc-haskell)

;;; hc-haskell.el ends here

