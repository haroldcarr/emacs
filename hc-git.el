;;; hc-git.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;; Version Control
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html#Version-Control
;; - http://emacswiki.org/emacs/VersionControl
;; GIT
;; - http://magit.github.com/magit/magit.html
;; - http://www.emacswiki.org/emacs/Magit
;; - https://github.com/pidu/git-timemachine
;; Magit and Ediff
;; - http://dachary.org/?p=2893

;;; Code:

(eval-when-compile (require 'use-package))

;; WORKAROUND
;; https://www.reddit.com/r/emacs/comments/1h4xjts/magit_issues/
(defvar git-commit-filename-regexp nil)

(use-package magit
  :config
  (progn
    (setq git-commit-summary-max-length 80)
    (setq git-commit-fill-column        80)
  )
)

;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  :config
  (progn
    (global-git-gutter-mode t)
    ;; HC: enabling this breaks git-gutter
    ;; to use git-gutter and linum-mode
    ;;(git-gutter:linum-setup)
  )
)

(provide 'hc-git)

;;; hc-git.el ends here
