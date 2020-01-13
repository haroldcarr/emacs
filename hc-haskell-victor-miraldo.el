;; https://github.com/VictorCMiraldo/victor-emacs-config/blob/master/local/haskell.el
;; My haskell-mode niceties

;; We need haskell-mode!
(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(comment
(require 'ac-haskell-process)
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))
)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; makes sure emacs knows what are haskell files
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; makes sure that evil starts interactive-haskell in normal mode
;;
;; this is not working... i have no idea why
;;
;; (with-eval-after-load 'evil-mode
;;   (add-to-list 'evil-insert-state-modes 'haskell-cabal-mode)
;;   (add-to-list 'evil-insert-state-modes 'haskell-interactive-mode)
;;   (add-to-list 'evil-insert-state-modes 'interactive-haskell-mode)
;; )

;; * haskell-mode commands

(comment
(defun haskell-process-kill ()
  "Kill haskell process. Very usefull when it starts leaking"
  (interactive)
  (kill-process (haskell-process-process (haskell-commands-process))))
;; Making some alignment magic

(unless (bound-and-true-p align-rules-list)
  (setq align-rules-list '())
)
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

;; My own haskell-align
(defun haskell-align (start end)
  (interactive "r")
  (align start end nil my-align-rules-list))
)
;;
;; Some haskell-mode functionality keybindings.
;;
(comment
(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "M-[")         'align)
  (define-key haskell-mode-map (kbd "<f8>")        'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c M-e")     'haskell-goto-first-error)
  (define-key haskell-mode-map (kbd "M-n")         'haskell-goto-next-error)
  (define-key haskell-mode-map (kbd "M-p")         'haskell-goto-prev-error)
  (define-key haskell-mode-map (kbd "M-g")         'haskell-mode-jump-to-def-or-tag)
  (define-key haskell-mode-map (kbd "C-c C-l")     'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-k")     'haskell-process-kill)
  (define-key haskell-mode-map (kbd "C-c C-z")     'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t")     'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i")     'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c")   'haskell-process-cabal))

(with-eval-after-load 'haskell-interactive-mode
  (define-key haskell-interactive-mode-map (kbd "C-c k !") 'haskell-process-kill)
)

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal))
)

(provide 'hc-haskell-victor-miraldo)
