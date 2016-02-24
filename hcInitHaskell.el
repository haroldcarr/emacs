;;; hcInitHaskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2016 Feb 17 (Wed) 14:44:15 by Harold Carr.
;;;;

;;; Code:

;; COMMON
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (custom-set-variables
               ;; put a version of ghc on path:
               ;; see: https://github.com/kazu-yamamoto/ghc-mod/issues/660
               '(haskell-process-type 'auto)
               '(haskell-indent-spaces 4)
               '(haskell-indentation-layout-offset 4)
               '(haskell-indentation-left-offset 4)
               '(haskell-indentation-ifte-offset 4)
               '(haskell-process-auto-import-loaded-modules t)
               '(haskell-process-suggest-remove-import-lines t)
               ;; CURRENT: stack install stylish-haskell
               ;;          put .local/bin in PATH
               '(haskell-stylish-on-save t)
               '(haskell-tags-on-save t)
               '(haskell-process-log t)
               )
              (turn-on-haskell-doc)
              (turn-on-haskell-decl-scan)
              )))

(provide 'hcInitHaskell)

;;; hcInitHaskell.el ends here

