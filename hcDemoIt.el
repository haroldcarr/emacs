;;; hcDemoIt --- packages for doing demos

;;; Commentary:

;;; Code:

(usepackage demoit
  :defer t)

(usepackage expandregion
  :defer t
  :bind ("C=" . er/expandregion))

(usepackage fancynarrow
  :defer t)

(usepackage org
  :defer t
  :init
  (progn
    (setq orghideemphasismarkers t
          orglogdone 'time
          orgsrcfontifynatively t
          orgstartuptruncated nil))
  :config
  (progn
    (progn
      (orgbabeldoloadlanguages
       'orgbabelloadlanguages
       '((emacslisp . t)
         (sh . t))))))

(usepackage orgbullets
  :defer t
  :init
  (progn
    (addhook 'orgmodehook #'orgbulletsmode)))

(usepackage orgtreeslide
  :defer t)

(use-package zenburn-theme
  :ensure t
  :demand
  :init
  (progn
    ;; Increase contrast for presentation.
    (defvar zenburn-override-colors-alist
      '(("zenburn-bg-1"     . "#101010")
        ("zenburn-bg-05"    . "#202020")
        ("zenburn-bg"       . "#2B2B2B")
        ("zenburn-bg+05"    . "#383838")
        ("zenburn-bg+1"     . "#3F3F3F")
        ("zenburn-bg+2"     . "#494949")
        ("zenburn-bg+3"     . "#4F4F4F")))
    (load-theme 'zenburn 'no-confirm)))

(provide 'hcDemoIt)

;;; hcDemoIt.el ends here
