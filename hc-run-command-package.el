;;; hc-run-command-package.el --- Summry run-command-package

;;; Commentary:

;;; Code:

;; a failed experiment to have no buffer and no output
;; ;; https://macowners.club/posts/custom-functions-1-baseline/
;; (defun timu-baseline-async-shell-command-no-window (command _buffer-base-name _output-buffer)
;;   "Do not display the `async-shell-command' COMMAND output buffer.
;; Credit: https://stackoverflow.com/a/60333836
;; Credit: https://stackoverflow.com/a/47910509."
;;   (interactive)
;;   (let ((display-buffer-alist '(("\\*Async Shell Command\\*.*" display-buffer-no-window))))
;;     (async-shell-command command)))

(defun run-command-open ()
  (list
   (when-let ((filename (thing-at-point 'filename t)))
     (list
      :command-name "Open"
      :command-line (format "open '%s'" filename)
      :display (format "open '%s'" filename)
      :runner 'run-command-runner-compile))
   (when-let ((filename (thing-at-point 'filename t)))
     (list
      :command-name "Quick Look"
      :command-line (format "qlmanage -p '%s'" filename)
      :display (format "qlmanage -p '%s'" filename)
      :runner 'run-command-runner-compile))
  ))

(use-package run-command
  :custom
  (run-command-recipes '(run-command-open))
)

(provide 'hc-run-command-package)

;;; hc-run-command-package.el ends here
