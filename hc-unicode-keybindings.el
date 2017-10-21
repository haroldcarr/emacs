;;; hc-unicode-keybindings --- Unicode key bindings

;;; Commentary:
;; http://ergoemacs.org/emacs/emacs_n_unicode.html

;; ¬∧∨∃⊦∵∴∈∉⊂⊃⊆⊇⊄⋂⋃

;;; Code:

;; (global-set-key (kbd "<f5>")   (lambda () (interactive) (find-file (concat (hcEmacsDir) "/unicode.txt"))))
(global-set-key (kbd "<f6> /") (lambda () (interactive) (insert "λ")))
(global-set-key (kbd "<f6> <") (lambda () (interactive) (insert "←")))
(global-set-key (kbd "<f6> >") (lambda () (interactive) (insert "→")))
(global-set-key (kbd "<f6> a") (lambda () (interactive) (insert "∀")))
(global-set-key (kbd "<f6> e") (lambda () (interactive) (insert "∃")))
(global-set-key (kbd "<f6> 0") (lambda () (interactive) (insert "∅")))

(provide 'hc-unicode-keybindings)

;;; hc-unicode-keybindings.el ends here
