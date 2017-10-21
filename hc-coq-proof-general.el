;;; hc-cog-proof-general --- packages for doing bookmark

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat (hcEmacsDir) "/ProofGeneral/generic"))

(use-package proof-site
  :defer t
  :config
  (progn
    (setq coq-prog-name "/Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqtop")
    (setq proof-splash-time 2))
)

(provide 'hc-coq-proof-general)

;;; hc-coq-proof-general.el ends here

