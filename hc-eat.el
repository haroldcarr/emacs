;;; hc-eat --- packages for emacs-eat

;;; Commentary:

;; https://codeberg.org/akib/emacs-eat
;; Emulate A Terminal, in a region, in a buffer and in Eshell

;;; Code:

(eval-when-compile (require 'use-package))

;; https://www.emacswiki.org/emacs/BookmarkPlus

(use-package quelpa-use-package)

(use-package eat
  :quelpa (eat :fetcher git
               :url "https://codeberg.org/akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el"))))

(provide 'hc-eat)

;;; hc-eat.el ends here

