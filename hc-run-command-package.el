;;; hc-run-command-package.el --- Summry run-command-package

;;; Commentary:

;;; Code:

(defun run-command-musescore ()
  (list
   (let* ((filename (thing-at-point 'filename t))
          (ext (file-name-extension filename)))
     (when (equalp ext "mscz")
       (list
        :command-name "MuseScore"
        :command-line (format "/Applications/MuseScore?4?2.app/Contents/MacOS/mscore '%s'" filename)
        :display (format "Launch MuseScore on '%s'" filename)
        :runner 'run-command-runner-vterm)))
   ))

(use-package run-command
  :custom
  (run-command-recipes '(run-command-musescore))
)

(provide 'hc-run-command-package)

;;; hc-run-command-package.el ends here
