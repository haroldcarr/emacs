;;; hc-compilation.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Building.html#Building
;; - see Compilation, Compilation Mode and Compilation Shell
;; http://emacswiki.org/emacs/CompilationMode

;; For extending it to work with Maven:

;; http://praveen.kumar.in/2011/03/09/making-gnu-emacs-detect-custom-error-messages-a-maven-example/

(use-package compile
  :defer t
  :config
  (add-to-list 'compilation-error-regexp-alist 'maven)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(maven "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
                       1 2 3)))


(provide 'hc-compilation)

;;; hc-compilation.el ends here
