;;; hc-racket-mode.el --- Summry racket-mode

;;; Commentary:

;; https://github.com/greghendershott/racket-mode

;;; Code:

(eval-when-compile (require 'use-package))

(use-package racket-mode
  :config (progn
            (setq racket-program "/Applications/Racket v6.9/bin/racket")
          )
)

(provide 'hc-racket-mode)

;;; hc-racket-mode.el ends here
