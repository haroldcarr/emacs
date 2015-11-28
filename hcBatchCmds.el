;;; hcBatchCmds --- for emacs --batch
;;; Commentary:
;;; Code:

(defun hc-org-export-as (export-type infilename outfilename)
  "EXPORT-TYPE : html, latex.
INFILENAME : .
OUTFILENAME : ."
  (print 'START)
  (add-to-list 'load-path (shell-command-to-string "hcLocation emacs"))
  (require '.emacs.common)
  (require 'hcInitOrgMode)
  (find-file infilename)
  (let ((exported (org-export-as export-type))
        (save-silently-p t))
    (print (concat "Opening: " outfilename))
    (with-temp-file outfilename (insert exported)))
  (print 'DONE))

(provide 'hcBatchCmds)

;;; hcBatchCmds.el ends here

