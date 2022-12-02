;;; .init --- minimum Haskell development environment

;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives
      '(
        ("melpa"  . "http://melpa.org/packages/")
        ;; https://list.orgmode.org/87lfa7tc9v.fsf@gnu.org/t/
        ("gnu"    . "http://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ;;("org"    . "http://orgmode.org/elpa/")
       ))

(package-initialize)

(use-package which-key
  :ensure nil
  :demand
  :pin melpa
  :init (which-key-mode))

;; add the location of the files below
(add-to-list 'load-path (shell-command-to-string "hcLocation emacs"))

(use-package "hc-company.el")
(use-package "hc-helm.el")
(use-package "hc-yasnippet.el")
(use-package "hc-projectile.el")
(use-package "hc-haskell.el")
(use-package "hc-haskell-hls.el")


;;; .init.el ends here

