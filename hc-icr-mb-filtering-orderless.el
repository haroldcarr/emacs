;;; hc-icr-mb-filtering-orderless.el --- hc-icr-mb-filtering-orderless   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------------------------------------------------------
;; ORDERLESS : The Filtering Layer
;; https://github.com/oantolin/orderless

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)

  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  ;; (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(provide 'hc-icr-mb-filtering-orderless)

;;; hc-icr-mb-filtering-orderless.el ends here
