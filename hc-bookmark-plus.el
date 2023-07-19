;;; hc-bookmark-plus --- packages for doing bookmark

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package bookmark+
  :ensure nil
  :config (setq bmkp-prompt-for-tags-flag t))

(provide 'hc-bookmark-plus)

;;; hc-bookmark-plus.el ends here

