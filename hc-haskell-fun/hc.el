;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start

(haskell-emacs-init)
;; inside
   (haskell-emacs--start-proc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stop

(haskell-emacs--stop-proc)

;; inside
    haskell-emacs--proc
    (set-process-sentinel haskell-emacs--proc nil)
    (kill-process haskell-emacs--proc)
    (setq haskell-emacs--proc nil)

;; manual (but screws up haskell-emacs state)
(list-processes)
(setq pp (get-process "hask<1>"))
(kill-process pp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use

(Matrix.identity 3)
  ;;=> ((1 0 0) (0 1 0) (0 0 1))
(Matrix.transpose '((1 2) (3 4) (5 6)))
  ;;=> ((1 3 5) (2 4 6))
(Matrix.isIdentity '((1 0) (0 1)))
  ;;=> t
(Matrix.dyadic '(1 2 3) '(4 5 6))
  ;;=> ((4 5 6) (8 10 12) (12 15 18))
;; bad input:
(Matrix.identity "a")
  ;;=> Debugger entered--Lisp error: (error "when expecting a Integral, encountered string instead")
(Matrix.transpose [(1 2) [3 4]])
  ;;=> ((1 3) (2 4))
(Matrix.dyadic '+)
  ;; => Debugger entered--Lisp error: (error "when expecting a pair, encountered symbol instead")
  ;; => ((1 0 0) (0 1 0) (0 0 1))
(HC.hello "Harold")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; how it works: defines macros

(print (macroexpand '(Matrix.identity 3)))

(progn (process-send-string haskell-emacs--proc (format "%S" (haskell-emacs--optimize-ast (quote (Matrix\.identity 3))))) (haskell-emacs--get 0))

;; end
