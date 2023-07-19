;;; hc-yasnippet --- -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package yasnippet
  :ensure nil
  :demand
  :diminish (yas-minor-mode . "")
  :init
  (progn
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
    (yas-global-mode))
  :config
  (progn
    (defun init-yas-uncapitalize (cap)
      (concat (downcase (substring cap 0 1))
              (substring cap 1)))

    (unbind-key "TAB"   yas-minor-mode-map)
    (unbind-key "<tab>" yas-minor-mode-map)))

(bind-key "M-/" #'hippie-expand)

(provide 'hc-yasnippet)

;;; hc-yasnippet.el ends here
