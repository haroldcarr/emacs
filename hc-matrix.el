;;; hc-matrix.el --- matrix          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://github.com/alphapapa/ement.el

(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; (not on MELPA yet)
(use-package plz
  :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

(use-package ement
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

(provide 'hc-matrix)

;;; hc-matrix.el ends here
