;;; hc-icr-ib-completions-cape.el --- hc-icr-ib-completions-cape          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------------------------------------------------------
;; CAPE : The In-Buffer Backend Layer (completions)
;; Corfu is frontend : how completions are displayed
;; Cape  is backend  : what completions are available
;; https://github.com/minad/cape

(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)      ; from current buffers
  ;;(add-hook 'completion-at-point-functions #'cape-dictionary) ; from a dictionary file
  (add-hook 'completion-at-point-functions #'cape-file)         ; file paths
  ;;(add-hook 'completion-at-point-functions #cape-emoji)       ; what it says
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol) ; elisp symbols anywhere (comments and docs too)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)      ; programming language keywords
  ;;(add-hook 'completion-at-point-functions #'cape-line)       ; complete entire ines from the buffer
  ;; ...
)

(setq completion-styles '(orderless basic)
      completion-category-overrides
      '((file (styles basic partial-completion))))

(provide 'hc-icr-ib-completions-cape)

;;; hc-icr-ib-completions-cape.el ends here
