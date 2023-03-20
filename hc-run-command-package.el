;;; hc-run-command-package.el --- Summry run-command-package

;;; Commentary:

;;; Code:

(defun run-command-open ()
  (list
   (when-let ((filename (thing-at-point 'filename t)))
     (list
      :command-name "Open"
      :command-line (format "open '%s'" filename)
      :display (format "open '%s'" filename)
      :runner 'run-command-runner-vterm))
  ))

(use-package run-command
  :custom
  (run-command-recipes '(run-command-open))
)

(provide 'hc-run-command-package)

;;; hc-run-command-package.el ends here
