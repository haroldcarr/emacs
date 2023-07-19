;;; hc-demo-it --- packages for doing demos

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package demo-it
  :defer t)

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package fancy-narrow
  :defer t)

(use-package org
  :defer t
  :init
  (progn
    (setq org-hide-emphasis-markers t
          org-log-done 'time
          org-src-fontify-natively t
          org-startup-truncated nil))
  :config
  (progn
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (sh . t))))))

(use-package org-bullets
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook #'org-bullets-mode)))

(use-package org-tree-slide
  :defer t)

(use-package zenburn-theme
  :demand
  :init
  (progn
    ;; Increase contrast for presentation.
    (defvar zenburn-override-colors-alist
      '(("zenburn-bg-1"     . "#101010")
        ("zenburn-bg-05"    . "#202020")
        ("zenburn-bg"       . "#2B2B2B")
        ("zenburn-bg+05"    . "#383838")
        ("zenburn-bg+1"     . "#3F3F3F")
        ("zenburn-bg+2"     . "#494949")
        ("zenburn-bg+3"     . "#4F4F4F")))
    (load-theme 'zenburn 'no-confirm)))

(provide 'hc-demo-it)

;;; hc-demo-it.el ends here
