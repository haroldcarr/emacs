(eval-when-compile (require 'use-package))
(add-to-list 'custom-theme-load-path (concat (hcEmacsDir) "/.vanilla.emacs.d/elpa/curry-on-theme-20210322.1717"))
(use-package curry-on-theme :demand :init (load-theme 'curry-on 'no-confirm))
