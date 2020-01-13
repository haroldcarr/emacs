(if (locate-file "agda-mode" exec-path exec-suffixes 1)
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
  (message "agda-mode not on path"))

(provide 'hc-agda)

