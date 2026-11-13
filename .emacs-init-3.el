;;; Commentary:

;;; Code:
;; ------------------------------------------------------------------------------
;; END STARTUP
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000000))

(provide '.vanilla.emacs)

