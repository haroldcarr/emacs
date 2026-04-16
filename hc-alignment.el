;;; hc-alignment.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; https://gist.github.com/700416
;; http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
;; - uses http://www.emacswiki.org/emacs/rx

(defmacro hcMakeAlignCmd (name char) "NAME CHAR."
  `(defun ,name (begin end)
     ,(concat "Align region to " char " signs")
     (interactive "r")
     (align-regexp begin end
                   (rx (group (zero-or-more (syntax whitespace))) ,char)
                   1 1)))

(hcMakeAlignCmd align-to-colon        ":")
(hcMakeAlignCmd align-to-equals       "=")
(hcMakeAlignCmd align-to-hash         "=>")
(hcMakeAlignCmd align-to-comma-before ",")

(defun align-to-comma-after (begin end)
  "BEGIN END: Align region to , signs."
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(provide 'hc-alignment)

;;; hc-alignment.el ends here
