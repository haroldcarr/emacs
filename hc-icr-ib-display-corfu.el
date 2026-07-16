;;; hc-icr-ib-display-corfu.el --- hc-icr-ib-display-corfu          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------------------------------------------------------
;; CORFU : The In-Buffer Display Layer (completion)
;; https://github.com/minad/corfu

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; HC
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 2)

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
)

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(cond ((not (equalp (hcMachineName) "o2011"))
       (use-package dabbrev
         ;; Swap M-/ and C-M-/
         ;;:bind (("M-/" . dabbrev-completion)
         ;;       ("C-M-/" . dabbrev-expand))
         :config
         (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
         (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
         (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
         (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
         (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))))

(provide 'hc-icr-ib-display-corfu)

;;; hc-icr-ib-display-corfu.el ends here
