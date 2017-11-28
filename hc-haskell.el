;;; hc-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2017 Nov 02 (Thu) 08:15:14 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

;; for Intero, see: https://github.com/cydparser/demo-emacs-haskell/blob/master/demo.org

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

(defvar hc-haskell)

(cond ((y-or-n-p-with-timeout "for Haskell: use Dante/y; Intero/n (the default)" 6 nil)
       (message "Using Dante")
       (setq hc-haskell 'dante)
       (use-package hc-haskell-dante))
      (t
       (message "Using Intero")
       (setq hc-haskell 'intero)
       (use-package hc-haskell-intero)))

(provide 'hc-haskell)

;;; hc-haskell.el ends here

