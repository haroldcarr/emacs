;;; hc-haskell-w3m-haddocks --- Summary

;;; Commentary:

;; https://github.com/haskell/haskell-mode/wiki/Browsing-Haddocks

;;; Code:

(eval-when-compile (require 'use-package))

(use-package w3m
  :defer t
  :config
  (setq w3m-mode-map (make-sparse-keymap))

  (define-key w3m-mode-map (kbd "RET")       'w3m-view-this-url)
  (define-key w3m-mode-map (kbd "q")         'bury-buffer)
  (define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
  (define-key w3m-mode-map [f5]              'w3m-reload-this-page)
  (define-key w3m-mode-map (kbd "C-c C-d")   'haskell-w3m-open-haddock)
  (define-key w3m-mode-map (kbd "M-<left>")  'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "M-.")       'w3m-haddock-find-tag)

  (defun w3m-maybe-url ()
    (interactive)
    (if (or (equal '(w3m-anchor)         (get-text-property (point) 'face))
            (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
        (w3m-view-this-url)))
)

;; C-c C-d to prompt for package to browse
(use-package w3m-haddock
  :defer t
  :config
  (add-hook 'w3m-display-hook 'w3m-haddock-display)
  (setq haskell-w3m-haddock-dirs '("~/Library/Haskell/doc/"
                                   "~/CABAL-SANDBOXES/Beginning_Haskell/share/doc/"
                                   "~/CABAL-SANDBOXES/bitly-client/share/doc/"
                                   "~/CABAL-SANDBOXES/graphviz/share/doc/"
                                   "~/CABAL-SANDBOXES/hsparql/share/doc/"
                                   "~/CABAL-SANDBOXES/tim-dysinger-env/share/doc/"
                                   ))
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
)

(provide 'hc-haskell-w3m-haddocks)

;;; hc-haskell-w3m-haddocks.el ends here
