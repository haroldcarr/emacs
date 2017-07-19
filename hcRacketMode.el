;;; hcRacketMode.el --- Summry racketmode

;;; Commentary:

;;; Code:

(with-no-warnings
(use-package racket-mode
  :config (progn (setq racket-program "/Applications/Racket v6.9/bin/racket")
          )
)

)

(provide 'hcRacketMode)
;;; hcRacketMode.el ends here
