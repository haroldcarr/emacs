;;; hc-mode-line --- my powerline

;;; Commentary:

;;; Code:

(use-package spaceline
  :ensure t
  :config (require 'spaceline-config))

(defvar hc-mode-line-left
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

(defvar hc-mode-line-right
  '(which-function
    (erc-track :when active)            ;; new messages in IRC channel
    (global :when active)))             ;; ??

(defun hc-mode-Line ()
  "."
  (interactive)
  (spaceline-install hc-mode-line-left hc-mode-line-right)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(hc-mode-Line)

(provide 'hc-mode-line)

;;; hc-mode-line.el ends here
