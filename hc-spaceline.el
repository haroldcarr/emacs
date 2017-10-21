;;; hc-spaceline --- my powerline

;;; Commentary:

;;; Code:

(use-package spaceline
  :ensure t
  :config (require 'spaceline-config))

(defvar hc-spaceline-left
  '(((persp-name workspace-number window-number)
     :separator "|"
     :face highlight-face)
    anzu                                ;; current match/number of matches
    (buffer-modified                    ;; asterisk when modified
     buffer-id                          ;; the name
     remote-host)                       ;; the host for remote buffers
    ((line-column                       ;; L:C
      buffer-position                   ;; Top | percentage | Bottom
      hud                               ;; graphic that shows position in buffer (when not at Top or Bottom)
      buffer-size                       ;; total size
      buffer-encoding-abbrev)           ;; e.g., 'unix', 'utf-8'
     :separator " | ")
    input-method                        ;; ??
    selection-info                      ;; number of characters or lines selected
    major-mode                          ;; current major mode e.g., Emacs-Lisp, LitHaskell
    ((flycheck-error                    ;; number of errors
      flycheck-warning                  ;; number of warnings
      flycheck-info)                    ;; number of notifications
     :when active)
    ;;(((minor-modes                      ;; currently enabled minor modes
    ;;   :separator spaceline-minor-modes-separator) process) :when active)
    (version-control :when active)      ;; e.g., GIT, ...
    ))

(defvar hc-spaceline-right
  '(which-function
    (erc-track :when active)            ;; new messages in IRC channel
    (global :when active)))             ;; ??

(defun hcSpaceline ()
  "."
  (spaceline-install hc-spaceline-left hc-spaceline-right)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(hcSpaceline)

(provide 'hc-spaceline)

;;; hc-spaceline.el ends here
