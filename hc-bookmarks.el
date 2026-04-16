;;; hc-bookmarks --- packages for doing bookmark           -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))


;; - Registers
;;   - http://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html#Registers
;;   - http://emacswiki.org/emacs/Registers
;; - Bookmarks (like registers, but persistent)
;;   - http://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html#Bookmarks
;;   - http://emacswiki.org/emacs/BookMarks

;; TODO
;; - http://www.emacswiki.org/emacs-en/BookmarkPlus

(defvar bookmark-save-flag)
(setq   bookmark-save-flag 1)
(defvar bookmark-sort-flag)
(setq   bookmark-sort-flag nil)
(defvar bookmark-default-file)
(setq   bookmark-default-file (concat (hcEmacsDir) "/.emacs.bmk"))

;; ------------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/BookmarkPlus

;; (use-package quelpa-use-package)

;; (use-package bookmark+
;;   ;; :quelpa (bookmark+ :fetcher wiki
;;   ;;                    :files
;;   ;;                    ("bookmark+.el"
;;   ;;                     "bookmark+-mac.el"
;;   ;;                     "bookmark+-bmu.el"
;;   ;;                     "bookmark+-1.el"
;;   ;;                     "bookmark+-key.el"
;;   ;;                     "bookmark+-lit.el"
;;   ;;                     "bookmark+-doc.el"
;;   ;;                     "bookmark+-chg.el"))
;;   ;; :defer 2
;;   :ensure nil
;;   :config (setq bmkp-prompt-for-tags-flag t))

(provide 'hc-bookmarks)

;;; hc-bookmarks.el ends here

