;;; hcInitHaskellSpacemacs --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2015 Nov 26 (Thu) 13:51:59 by Harold Carr.
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
               '(haskell-process-type 'stack-ghci)
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

(provide 'hcInitHaskellSpacemacs)

;;; hcInitHaskellSpacemacs.el ends here

