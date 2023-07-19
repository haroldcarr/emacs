(eval-when-compile (require 'use-package))

(use-package nimbus-theme :demand :init (load-theme 'nimbus 'no-confirm)
  :config (set-face-background 'default "gray14"))

