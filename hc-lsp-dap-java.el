;;; hc-lsp-dap-java.el --- rust          -*- lexical-binding: t; -*-

;;; Commentary:
;; based on
;; https://www.youtube.com/watch?v=_Ilw8k_zHT0
;; and
;; https://www.youtube.com/watch?v=Yah69AfYP34&t=685s
;; https://talks.skybert.net/emacs-java-setup/emacs-java.html

;;; Code:

(use-package projectile)
(use-package flycheck)
(use-package yasnippet
  :config (yas-global-mode))
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-java-server-install-dir (concat (hcEmacsDir) "/JDTLS/"))
  (setq lsp-java-java-path          "/usr/bin/java")
)
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key
  :config (which-key-mode))
(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode
  :after lsp-mode
;;  :config (dap-auto-configure-mode)
  )
(use-package dap-java
  :ensure nil)
;(use-package helm-lsp)
;(use-package helm
;  :config (helm-mode))
(use-package lsp-treemacs)

(provide 'hc-lsp-dap-java)

;;; hc-lsp-dap-java.el ends here
