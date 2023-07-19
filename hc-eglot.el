;;; hc-eglot --- Summary

;;; Commentary:

;;;;
;;;; Created       : 2023 Mar 08 (Wed) 10:34:09 by Harold Carr.
;;;; Last Modified : 2023 Jul 18 (Tue) 18:37:37 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

;; https://joaotavora.github.io/eglot/

;;(defvar hc-eglot-mode-keymap-prefix "C-c e")

(use-package eglot
  :config
  ;;(define-key eglot-mode-map (kbd hc-eglot-mode-keymap-prefix) eglot-mode-map)
  :bind (:map eglot-mode-map
              ("C-c e G r"   . xref-find-references)
              ("C-c e h h"   . eldoc)
              ("C-c e f b"   . flymake-show-buffer-diagnostics)
              ("C-c e f p"   . flymake-show-project-diagnostics)
        )
)

;; TODO : which key integration

;; if eglot has a hard time finding typescript-language-server:
;;
;; (with-eval-after-load 'eglot
;;   ;; Find paths to the executables (already installed).
;;   (let ((path-to-typescript-lib-dir
;;          (expand-file-name "~/.config/yarn/global/node_modules/typescript-language-server/lib"))
;;         (path-to-typescript-language-server (executable-find "typescript-language-server")))
;;     (add-to-list
;;      'eglot-server-programs
;;      `((js-mode typescript-mode) .
;;        (,path-to-typescript-language-server
;;         "--stdio"
;;         "--tsserver-path" ,path-to-typescript-lib-dir)))))

(provide 'hc-eglot)

;;; hc-eglot.el ends here

