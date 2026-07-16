;;; hc-spelling.el --- xxx          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://emacsredux.com/blog/2026/07/13/replacing-flyspell-with-jinx/

;; enchant installed in home-manager and .bashrc

;; (use-package jinx
;;   :custom-face
;;   ;; Available styles:  line, double-line, wave, dots, dashes
;;   (jinx-misspelled
;;    ((t (:underline (:color "white"
;;                     :style dashes)))))
;;   :hook (emacs-startup . global-jinx-mode)
;;   :bind (("M-$"   . jinx-correct)
;;          ("C-M-$" . jinx-languages)))

(provide 'hc-spelling)

;;; hc-spelling.el ends here
