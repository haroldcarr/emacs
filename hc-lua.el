;;; hc-lua.el --- xxx          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server"))))

(provide 'hc-lua)

;;; hc-lua.el ends here
