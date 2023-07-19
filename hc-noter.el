;;; hc-noter.el --- matrix          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package org-noter  ;; https://github.com/weirdNox/org-noter
                        ;; https://www.youtube.com/watch?v=lCc3UoQku-E
  :config
  (custom-set-variables '(org-noter-default-heading-title       "p $p$")
                        '(org-noter-hide-other                  nil)
                        '(org-noter-always-create-frame         nil)
                        '(org-noter-separate-notes-from-heading t)
                        '(org-noter-default-notes-file-names    "00-Notes.org")
                        '(org-noter-doc-property-in-notes       t)
                        '(org-noter-insert-note-no-questions    t))
)

(provide 'hc-noter)

;;; hc-noter.el ends here
