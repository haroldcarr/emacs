;;; hc-neotree.el --- Summry neotree

;;; Commentary:

;;; Code:

(with-no-warnings
(use-package neotree
  :bind (:map neotree-mode-map
              ("a"     . neotree-stretch-toggle)
              ("TAB"   . neotree-stretch-toggle)
              ("C-x 1" . neotree-stretch-toggle)
              )
  :config (progn (setq neo-theme 'arrow)
                 (setq neo-window-width 12)
                 (setq neo-show-hidden-files t)))
)

(provide 'hc-neotree)

;;; hc-neotree.el ends here
