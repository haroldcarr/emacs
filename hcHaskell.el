;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2015 Sep 15 (Tue) 20:30:15 by Harold Carr.
;;;;

;; Haskell setup courtesy [[http://ioctl.it/posts/2015-07-03-stack-flycheck.html]]

(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(hcRequire haskell-mode

(flycheck-define-checker haskell-stack
  "A Haskell syntax and type checker using ghc.
See URL `http://www.haskell.org/ghc/'."
  :command ("stack" "ghc" "--" "-Wall" "-fno-code"
            (option-flag "-no-user-package-db"
                         flycheck-ghc-no-user-package-database)
            (option-list "-package-db" flycheck-ghc-package-databases)
            (option-list "-i" flycheck-ghc-search-path concat)
            ;; Include the parent directory of the current module tree, to
            ;; properly resolve local imports
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-mode "hs")
                    (`literate-haskell-mode "lhs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more not-newline)))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode literate-haskell-mode)
  :next-checkers ((warning . haskell-hlint)))

(defun haskell-mode-setup-hook ()
  (interactive)
  ;; style
  (turn-on-haskell-indent)
  (setq haskell-indent-spaces 4)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-ifte-offset 4)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-suggest-remove-import-lines t)

;; cabal update
;; cabal install stylish-haskell
;; HC: put link to it in my bin
  (setq haskell-stylish-on-save t)

  (setq haskell-tags-on-save t)
  (setq haskell-process-log t)

  (turn-on-haskell-doc)
  (turn-on-haskell-decl-scan)
  (setq flycheck-disabled-checkers '(haskell-ghc))
  (interactive-haskell-mode)
  (flycheck-select-checker 'haskell-stack))

(add-hook 'haskell-mode-hook 'haskell-mode-setup-hook)

)

(provide 'hcHaskell)

;;; End of file.

