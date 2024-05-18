;; https://github.com/agzam/browser-hist.el/

(use-package browser-hist
;;  :init
;;  (require 'embark) ; load Embark before the command (if you're using it)
  :config
  (setq browser-hist-default-browser 'chrome)
  :commands (browser-hist-search)
)

(defun hc-browser-history ()
  "Search browser history."
  (interactive)
  (let ((browser (let ((choices '("brave" "chrome")))
                   (message "%s" (ido-completing-read "which browser: " choices)))))
    (setq browser-hist-default-browser (intern browser))
    (browser-hist-search)))

(provide 'hc-browser-history)
