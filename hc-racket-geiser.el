;;; hc-racket-geiser.el --- Summry geiser

;;; Commentary:

;;; Code:

(with-no-warnings
  (use-package geiser
    :config (progn (setq geiser-racket-binary
                         "/Applications/Racket v6.9/bin/racket")
                   (setq geiser-racket-gracket-binary
                         "/Applications/Racket v6.9/bin/gracket-text")
            )
  )
)

(provide 'hc-racket-geiser)

;;; hc-racket-geiser.el ends here
