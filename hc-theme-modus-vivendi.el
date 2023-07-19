(eval-when-compile (require 'use-package))

(use-package modus-vivendi-theme :demand
  :init
  (progn
    (load-theme 'modus-vivendi 'no-confirm)
    (setq modus-themes-success-deuteranopia t
          modus-themes-diffs                'deuteranopia
          modus-themes-org-agenda           '((header-block . (scale-title variable-pitch))
                                              (header-date  . (grayscale workaholic bold-today))
                                              (event        . (accented scale-small))
                                              (scheduled    . uniform)
                                              (habit        . traffic-light-deuteranopia)))))
