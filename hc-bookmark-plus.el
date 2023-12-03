;;; hc-bookmark-plus --- packages for doing bookmark

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;; https://www.emacswiki.org/emacs/BookmarkPlus

(use-package quelpa-use-package)

(use-package bookmark+
  ;; :quelpa (bookmark+ :fetcher wiki
  ;;                    :files
  ;;                    ("bookmark+.el"
  ;;                     "bookmark+-mac.el"
  ;;                     "bookmark+-bmu.el"
  ;;                     "bookmark+-1.el"
  ;;                     "bookmark+-key.el"
  ;;                     "bookmark+-lit.el"
  ;;                     "bookmark+-doc.el"
  ;;                     "bookmark+-chg.el"))
  ;; :defer 2
  :ensure nil
  :config (setq bmkp-prompt-for-tags-flag t))

(provide 'hc-bookmark-plus)

;;; hc-bookmark-plus.el ends here

