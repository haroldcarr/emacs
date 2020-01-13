;; https://github.com/digital-asset/ghcide

(defvar hc-ghcide)

(defun hc-pick-ghcide-support ()
  "Prompt pick from a list."
  (interactive)
  (let ((choices '("eglot" "lsp-ui" "none")))
    (message "%s" (ido-completing-read "which ghcide?: " choices))))

(let ((pick (hc-pick-ghcide-support)))
  (cond ((equal pick "eglot")
         (message "Using eglot")
         (setq hc-ghcide 'eglot)
         (use-package eglot
           :ensure t
           :config
           (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp"))))
         )
        ((equal pick "lsp-ui")
         (message "Using lsp-ui")
         (setq hc-ghcide 'lsp-ui)
         (use-package flycheck
           :ensure t
           :init
           (global-flycheck-mode t))
         (use-package yasnippet
           :ensure t)
         (use-package lsp-mode
           :ensure t
           :hook (haskell-mode . lsp)
           :commands lsp)
         (use-package lsp-ui
           :ensure t
           :commands lsp-ui-mode)
         (use-package lsp-haskell
           :ensure t
           :config
           (setq lsp-haskell-process-path-hie "ghcide")
           (setq lsp-haskell-process-args-hie '())
           ;; Comment/uncomment this line to see interactions between lsp client/server.
           (setq lsp-log-io t))
         )
        ((equal pick "none")
         (message "NO GHCIDE SUPPORT")
         (setq hc-ghcide 'none))
        (t
         (message "NO GHCIDE MATCH %s" pick))
        ))

(provide 'hc-haskell-ghcide)
