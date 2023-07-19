;;; hc-window-movement.el --- move                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;; SHIFT + arrow keys to move
(windmove-default-keybindings)

(use-package ace-window
    :bind (:map global-map
              ("C-x o" . 'ace-window)))

(provide 'hc-window-movement)

;;; hc-window-movement.el ends here
