;;; hc-lean --- Summary

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package lean-mode
  :demand)

;; M-.     jump to definition in source file (lean-find-definition)
;; M-,     jump back to position before M-. (xref-pop-marker-stack)
;; C-c C-k shows the keystroke needed to input the symbol under the cursor
;; C-c C-x execute lean in stand-alone mode (lean-std-exe)
;; C-c SPC run a command on the hole at point (lean-hole)
;; C-c C-d show a searchable list of definitions (helm-lean-definitions)
;; C-c C-g toggle showing current tactic proof goal (lean-toggle-show-goal)
;; C-c C-n toggle showing next error in dedicated buffer (lean-toggle-next-error)
;; C-c C-b toggle showing output in inline boxes (lean-message-boxes-toggle)
;; C-c C-r restart the lean server (lean-server-restart)
;; C-c C-s switch to a different Lean version via elan (lean-server-switch-version)
;; C-c ! n flycheck: go to next error
;; C-c ! p flycheck: go to previous error
;; C-c ! l flycheck: show list of errors

(use-package company-lean
  :demand)
(use-package helm-lean
  :demand)

(provide 'hc-lean)

;;; hc-lean.el ends here
