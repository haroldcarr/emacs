(eval-when-compile (require 'use-package))
;; https://protesilaos.com/emacs/ef-themes#h:ac76ded0-af9b-4566-aff9-75142ef2d4ef
(use-package ef-themes
  :init   (ef-themes-take-over-modus-themes-mode 1)
  :config (modus-themes-load-theme 'ef-deuteranopia-dark))


