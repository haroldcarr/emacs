;;; hc-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2019 Nov 09 (Sat) 11:50:34 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

;; for Intero, see: https://github.com/cydparser/demo-emacs-haskell/blob/master/demo.org

(require 'ido)

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

(defun hc-pick-haskell-support ()
  "Prompt pick from a list."
  (interactive)
  (let ((choices '("hie" "dante" "intero" "none")))
    (message "%s" (ido-completing-read "which haskell?: " choices))))

(let ((pick (hc-pick-haskell-support)))
  (cond ((equal pick "hie")
         (message "Using HIE")
         (setq hc-haskell 'hie)
         (use-package hc-haskell-hie))
        ((equal pick "dante")
         (message "Using Dante")
         (setq hc-haskell 'dante)
         (use-package hc-haskell-dante))
        ((equal pick "intero")
         (message "Using Intero")
         (setq hc-haskell 'intero)
         (use-package hc-haskell-intero))
        ((equal pick "none")
         (message "NO HASKELL SUPPORT")
         (setq hc-haskell 'none))
        (t
         (message "NO HASKELL MATCH %s" pick))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/knupfer/haskell-emacs

(use-package haskell-emacs
  :ensure t
  :defer t)

(setq haskell-emacs-build-tool (quote stack))
(setq haskell-emacs-dir (concat (hcEmacsDir) "/hc-haskell-fun/"))

;;;;;;;;;;;;;

(provide 'hc-haskell)

;;; hc-haskell.el ends here

