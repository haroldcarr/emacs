;;; hc-sparql-mode.el --- Summry sparql-mode

;;; Commentary:

;;; Code:

(with-no-warnings
  (use-package sparql-mode
    :config (sparql-set-base-url "http://localhost:3040/ds/query?default"))
)

(provide 'hc-sparql-mode)

;;; hc-sparql-mode.el ends here
