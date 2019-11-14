;; see ./pdf-tools.org

;;  activate it
(with-no-warnings
(when (require 'pdf-tools nil :noerror)
  (progn
    (pdf-tools-install)
    ;; THIS WORKS
    (setq pdf-annot-list-format
          '((page . 3)
            (type . 10)
            (label . 24)
            (contents . 56)))
    (global-set-key (kbd "C-c z") 'pdf-view-midnight-minor-mode)
  )
)

;; default
;;(setq pdf-annot-list-format
;;  '((page . 3)
;;    (type . 10)
;;    (label . 24)
;;    (date . 24)))

;; what I want but it does not work
;;(setq pdf-annot-list-format
;;  '((page . 3)
;;    (contents . 56)))

(provide 'hc-pdf)
