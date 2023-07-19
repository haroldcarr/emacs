;;; hc-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Jul 18 (Tue) 18:41:14 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(require 'ido)

(use-package hc-flycheck)

(use-package haskell-mode
  :ensure nil
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
  :ensure nil
  :defer t)

;;(use-package hlint-refactor
;;  :ensure nil
;;  :defer t
;;  :diminish ""
;;  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/knupfer/haskell-emacs

;; (use-package haskell-emacs
;;   :ensure nil
;;   :defer t)

;; (setq haskell-emacs-build-tool (quote stack))
;; (setq haskell-emacs-dir (concat (hcEmacsDir) "/hc-haskell-fun/"))

;;;;;;;;;;;;;

(provide 'hc-haskell)

;;; hc-haskell.el ends here

