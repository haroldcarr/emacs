;;; hc-racket-mode.el --- Summry racketmode

;;; Commentary:

;;; Code:

(with-no-warnings
  (use-package racket-mode
    :config (progn
              (setq racket-program "/Applications/Racket v6.9/bin/racket")
            )
  )
)

(provide 'hc-racket-mode)

;;; hc-racket-mode.el ends here
