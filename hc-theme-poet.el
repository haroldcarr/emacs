;;; hc-theme-poet --- Summary

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;(set-face-attribute 'default nil :family "Iosevka" :height 130)
;(set-face-attribute 'fixed-pitch nil :family "Iosevka")
;(set-face-attribute 'variable-pitch nil :family "Baskerville")

(add-hook 'text-mode-hook
           (lambda ()
            (variable-pitch-mode 1)))

;;(olivetti-mode 1)        ;; Centers text in the buffer - TODO: UNDEFINED
(flyspell-mode 1)        ;; Catch Spelling mistakes
;; (typo-mode 1)            ;; Good for symbols like em-dash - TODO: UNDEFINED
(blink-cursor-mode 0)    ;; Reduce visual noise
(linum-mode 0)           ;; No line numbers for prose

(defvar org-bullets-bullet-list '("◉" "○"))
;; (org-bullets 1)

(use-package poet-theme
  :ensure nil
  :demand
  :init (load-theme 'poet 'no-confirm))

(provide 'hc-theme-poet)

;;; hc-theme-poet.el ends here
