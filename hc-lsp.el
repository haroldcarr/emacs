;;; hc-lsp --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2023 Sep 05 (Tue) 09:07:54 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(defvar hc-lsp-mode-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix hc-lsp-mode-keymap-prefix))
                        (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd hc-lsp-mode-keymap-prefix) lsp-command-map)
  ;; https://emacs.stackexchange.com/a/54976/5176
  (setq lsp-enable-file-watchers nil
        lsp-enable-snippet       nil
        lsp-file-watch-threshold 512
        )
  :custom
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

  ;; these two should have the same value
  (lsp-auto-guess-root                t)
  (lsp-guess-root-without-session     t)
;;(lsp-completion-provider            :capf)
;;(lsp-completion-show-detail         t)
;;(lsp-completion-show-kind           t)
;;(lsp-diagnostics-provider           :auto)
  (lsp-eldoc-enable-hover             t)
  (lsp-enable-symbol-highlighting     t)
  (lsp-completion-enable-additional-text-edit nil) ;; do not auto insert missing imports
  (lsp-headerline-breadcrumb-enable   t)
  (lsp-lens-enable                    t)
  (lsp-modeline-code-actions-enable   t)
  (lsp-modeline-diagnostics-enable    t)
  ;; manual request via `lsp-signature-activate`
;;(lsp-signature-auto-activate        '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
)

(use-package lsp-ui
;;:custom-face ;; gray35/tango-dark; white/zenburn; autumn-light/mv/gray50
;;(lsp-ui-doc-background              ((t (:background "gray50"))))
  :custom
  (lsp-ui-doc-enable                 nil)
  (lsp-ui-doc-position               'top)
  (lsp-ui-doc-show-with-cursor       t)
  (lsp-ui-doc-show-with-mouse        t)
  (lsp-ui-sideline-enable            t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover        nil)
)

;;(require 'lsp-treemacs)


(provide 'hc-lsp)

;;; hc-lsp.el ends here

