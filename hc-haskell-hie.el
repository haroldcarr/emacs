;; https://github.com/haskell/haskell-ide-engine#using-hie-with-emacs
;;
;; git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
;; cd haskell-ide-engine
;;
;; Install for the version of GHC we are using in our OBM projects:
;;
;; stack ./install.hs hie-8.6.5
;; stack ./install.hs build-data
;;
;;
;; That should result in something like:
;;
;; ls ~/.local/bin
;; ...
;; -rwxr-xr-x   1 carr  staff  122539764 Nov  9 09:20 hie
;; -rwxr-xr-x   1 carr  staff  122539764 Nov  9 09:20 hie-8.6.5
;; -rwxr-xr-x   1 carr  staff    4195076 Nov  9 09:20 hie-wrapper
;;
;;
;; Install following emacs packages (using M-x list-packages)
;; - lsp-mode     ;; https://github.com/emacs-lsp/lsp-mode
;; - lsp-ui       ;; https://github.com/emacs-lsp/lsp-ui
;; - lsp-haskell  ;; https://github.com/emacs-lsp/lsp-haskell
;;
;;
;; Then add the following to your emacs configuration

(require 'lsp-mode)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(setq lsp-haskell-process-path-hie "hie-wrapper")

;; Then go visit a Haskell file and watch it work.

(provide 'hc-haskell-hie)

