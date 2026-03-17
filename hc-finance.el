;;; hc-finance --- Summary

;;; Commentary:

;; from https://hledger.org/editors.html

;;;;
;;;; Created       : 2026 Jan 12 (Mon) 16:34:37 by Harold Carr.
;;;; Last Modified : 2026 Jan 14 (Wed) 16:52:32 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(use-package ledger-mode
  :custom
  ((ledger-binary-path                          "/usr/local/bin/hledger") ;;"hledger.sh")
   (ledger-default-date-string                  "%Y-%m-%d")

   (ledger-mode-should-check-version            nil)
   (ledger-report-auto-width                    nil)
   (ledger-report-links-in-register             nil)
   (ledger-report-native-highlighting-arguments '("--color=always")))
  :mode
  ("\\.hledger\\'" "\\.ledger\\'"))

;; ------------------------------------------------------------------------------

;; To add extra checks for a specific journal file, add line like this near the top:
;; -*- eval:(add-to-list 'flymake-hledger-checks "recentassertions" t); -*-

;; Enable verbose use-package debug info when starting with --debug-init
;; (when init-file-debug
;;   (setq use-package-verbose t
;;         use-package-expand-minimally nil
;;         use-package-compute-statistics t
;;         debug-on-error t))

;; Configure flycheck-like keybindings for flymake:
(use-package flymake
  :bind (
         :map flymake-mode-map
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ;; a new list for each buffer, unlike flycheck
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! v" . flymake-switch-to-log-buffer)))

(use-package flymake-hledger
  :load-path "~/src/flymake-hledger"
  :after (ledger-mode flymake)

  :hook (
  (ledger-mode . flymake-hledger-enable)
  ;; Make C-x ` work ?
  ;; XXX Both of these work only in the first file opened; debugging needed.
  ;; (ledger-mode . (lambda () (setq next-error-function 'flymake-goto-next-error)))
  ;; (ledger-mode . (lambda () (setq next-error-function (lambda (num reset) (when reset (goto-char (point-min))) (flymake-goto-next-error num)))))
  )

  :custom
  (flymake-show-diagnostics-at-end-of-line t)  ; might require Emacs 30
  (flymake-suppress-zero-counters          t)
  ;; https://hledger.org/hledger.html#check
  ;;"recentassertions" "payees" "tags" "uniqueleafnames"
  (flymake-hledger-checks '("accounts" "commodities" "balancednoautoconversion" "ordereddates"))
  )

;; ------------------------------------------------------------------------------
;; ;; C-x ` steps to the next problem in the current file.
;; ;; C-u C-x ` restarts the scan from the top.

;; (use-package flycheck-hledger
;;   :after (flycheck ledger-mode)  ; or hledger-mode
;;   :ensure t
;;   :demand t
;;   :custom
;;   (flycheck-hledger-strict t)
;;   ;; extra checks from https://hledger.org/hledger.html#check:
;;   ;; ordereddates, uniqueleafnames, payees, recentassertions, tags..
;;   (flycheck-hledger-checks '("ordereddates" "recentassertions"))
;;   ;(flycheck-hledger-executable "hledger")
;; )

(provide 'hc-finance)

;;; hc-finance.el ends here

