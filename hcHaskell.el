;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2015 Sep 15 (Tue) 19:08:43 by Harold Carr.
;;;;

;; Haskell setup courtesy [[http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html]]

(hcRequire haskell-mode
  ;;(speedbar-add-supported-extension ".hs") ; chris done

  (autoload 'ghc-init "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (setq haskell-indent-spaces 4)

  ;;(setq haskell-mode-hook '(turn-on-haskell-indent))
  (setq haskell-tags-on-save t) ;; Chris Done
  (setq haskell-process-auto-import-loaded-modules t) ;; Chris Done
  (setq haskell-process-log t)
  (setq haskell-process-path-cabal "/Users/carr/Library/Haskell/bin/cabal")
  (setq haskell-process-path-ghci "stack ghci")
  (setq haskell-process-suggest-remove-import-lines t) ;; Chris Done
  (setq haskell-process-type 'ghci)

  ;; begin https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.el
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-ifte-offset 4)
  ;; end https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.el

  (setq haskell-stylish-on-save t)
  (setq haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"
                                          "--with-ghc=ghci-ng"))
  (define-key haskell-mode-map (kbd "C-x C-d") nil)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c M-.") nil)
;;(define-key haskell-mode-map (kbd "C-c C-d") nil) ;; set above in w3m
  (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)

  ;; BEGIN CHRIS DONE
  (define-key haskell-mode-map       (kbd "C-`")     'haskell-interactive-bring)
;;(define-key haskell-cabal-mode-map (kbd "C-`")     'haskell-interactive-bring)

  (define-key haskell-mode-map       (kbd "C-c C-c") 'haskell-process-cabal-build)
;;(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)

  (define-key haskell-mode-map       (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

  (define-key haskell-mode-map       (kbd "C-c c")   'haskell-process-cabal)
;;(define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal)

  (define-key haskell-mode-map       (kbd "SPC")     'haskell-mode-contextual-space)
  ;; END CHRIS DONE

  ;; Do this to get a variable in scope
  (auto-complete-mode)
  (defun hc-ac-haskell-candidates (prefix)
    (let ((cs (haskell-process-get-repl-completions (haskell-process) prefix)))
      (remove-if (lambda (c) (string= "" c)) cs)))
  (ac-define-source haskell
    '((candidates . (hc-ac-haskell-candidates ac-prefix))))
  (defun hc-haskell-hook ()
    (add-to-list 'ac-sources 'ac-source-haskell))
  (add-hook 'haskell-mode-hook 'hc-haskell-hook)

  ;; auto-complete-mode so can interact with inferior haskell and popup completion
  ;; I don't always want this.  Just turn on when needed.
  ;;(add-hook 'haskell-mode-hook (lambda () (auto-complete-mode 1)))
)

;;(use-package shm
;;  :defer t
;;  :config (add-hook 'haskell-mode-hook 'structured-haskell-mode))

(provide 'hcHaskell)

;;; End of file.

