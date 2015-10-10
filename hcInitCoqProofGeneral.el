;;; package --- COQ and ProofGeneral

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat (hcEmacsDir) "/ProofGeneral/generic"))

(use-package proof-site
  :defer t
  :config
  (setq coq-prog-name
        (cond ((hcWin32P) "/cygdrive/c/Program Files/Coq/bin/coqtop.exe")
              (t          "/Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqtop")))

  (setq proof-splash-time 2)
)

(provide hcInitCoqProofGeneral)
;;; hcInitCoqProofGeneral.el ends here

