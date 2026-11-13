;;; hc-forscore.el --- forscore          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hc-forscore-import (file)
  "Import FILE into forScore."
  (interactive "fPDF file: ")
  (unless (string-equal (downcase (or (file-name-extension file) "")) "pdf")
    (user-error "Not a PDF file: %s" file))
  (hc-forscore-import-aux file))

(with-eval-after-load 'embark
  (keymap-set embark-file-map "K" #'hc-forscore-import))

(defun hc-forscore-dired-import ()
  "Import the Dired file at point into forScore."
  (interactive)
  (hc-forscore-import (dired-get-file-for-visit)))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "K" #'hc-forscore-dired-import))

(defun hc-forscore-import-aux (full-path-file)
  (start-process "forScore" nil "open" "-a" "forScore"
                 (expand-file-name full-path-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find a common key

(require 'cl-lib)
(defun hc-unbound-keys (keymaps)
  "Return printable ASCII keys unbound in all KEYMAPS."
  (cl-loop for n from 33 to 126
           for key = (char-to-string n)
           when (cl-every
                 (lambda (map) (null (lookup-key map (kbd key))))
                 keymaps)
           collect key))

;; (require 'embark)
;; (require 'dired)
;; (hc-unbound-keys (list dired-mode-map embark-file-map))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hc-forscore)

;;; hc-forscore.el ends here
