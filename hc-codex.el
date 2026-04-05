;;; hc-codex --- for emacs --- lexical-binding   -*- lexical-binding: t -*-

;;; Commentary: https://github.com/bennfocus/codex-cli.el
;;; Code:

(use-package codex-cli
  ;; :ensure t
  ;; For GitHub (Emacs 29+):
  ;; :vc (:fetcher github :repo "bennfocus/codex-cli.el")
  :bind (("C-c c t" . codex-cli-toggle)
         ("C-c c s" . codex-cli-start)
         ("C-c c q" . codex-cli-stop)
         ("C-c c Q" . codex-cli-stop-all)
         ("C-c c p" . codex-cli-send-prompt)
         ("C-c c r" . codex-cli-send-region)
         ("C-c c f" . codex-cli-send-file)
         ;; Show-all layout + paging
         ("C-c c a" . codex-cli-toggle-all)
         ("C-c c n" . codex-cli-toggle-all-next-page)
         ("C-c c b" . codex-cli-toggle-all-prev-page))
  :init
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-side 'right
        codex-cli-width 90))

;;; Commentary: from ChatGPT.  Redundant (but doesn't depend on external package)
;;; Code:

(defvar hc-codex-command "codex"
  "Path to codex CLI.")

;; -----------------------------
;; Interactive Codex session

(defun hc-codex-shell ()
  "Start Codex in an interactive shell."
  (interactive)
  (let ((buf (get-buffer-create "*codex*")))
    (pop-to-buffer buf)
    (unless (comint-check-proc buf)
      (make-comint-in-buffer "codex" buf hc-codex-command))
    (with-current-buffer buf
      (comint-mode))))


(defun hc-codex-region (start end)
  "Send region to Codex for improvement (minimal diff)."
  (interactive "r")
  (shell-command-on-region
   start end
   (format "%s -p \"improve this code, minimal diff, preserve formatting\""
           hc-codex-command)))

(defun hc-codex-buffer ()
  "Send entire buffer to Codex."
  (interactive)
  (codex-region (point-min) (point-max)))

(defun hc-codex-prompt (prompt)
  "Run Codex with PROMPT."
  (interactive "sCodex prompt: ")
  (shell-command
   (format "%s -p \"%s\"" hc-codex-command prompt)))

;; -----------------------------
;; Rust

(defun hc-codex-rust-fix ()
  "Fix Rust build errors."
  (interactive)
  (shell-command
   (format "cargo build 2>&1 | %s -p \"fix this error minimal diff idiomatic rust no unwrap\""
           hc-codex-command)))

(defun hc-codex-rust-clippy-fix ()
  "Fix Rust clippy warnings."
  (interactive)
  (shell-command
   (format "cargo clippy 2>&1 | %s -p \"fix clippy warnings minimal diff idiomatic rust\""
           hc-codex-command)))

(defun hc-codex-rust-test-fix ()
  "Fix failing Rust tests."
  (interactive)
  (shell-command
   (format "cargo test 2>&1 | %s -p \"fix failing tests minimal diff rust\""
           hc-codex-command)))

;; -----------------------------
;; Haskell

(defun hc-codex-cabal-fix ()
  "Fix Haskell build errors."
  (interactive)
  (shell-command
   (format "cabal build 2>&1 | %s -p \"fix this minimal diff idiomatic haskell no partial functions\""
           hc-codex-command)))

(defun hc-codex-cabal-test-fix ()
  "Fix failing Haskell tests."
  (interactive)
  (shell-command
   (format "cabal test 2>&1 | %s -p \"fix failing tests minimal diff haskell\""
           hc-codex-command)))

;; -----------------------------
;; Test loop (generic)

(defun hc-codex-test-loop (cmd)
  "Run CMD (test command) through Codex loop."
  (interactive "sTest command: ")
  (shell-command
   (format "%s 2>&1 | %s -p \"fix failing tests minimal diff\""
           cmd hc-codex-command)))

;; -----------------------------
;; Keybindings (prefix: C-c c)
;; -----------------------------

;; (defvar hc-codex-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "r") #'codex-region)
;;     (define-key map (kbd "b") #'codex-buffer)
;;     (define-key map (kbd "p") #'codex-prompt)

;;     (define-key map (kbd "R") #'codex-rust-fix)
;;     (define-key map (kbd "C") #'codex-rust-clippy-fix)
;;     (define-key map (kbd "T") #'codex-rust-test-fix)

;;     (define-key map (kbd "H") #'codex-cabal-fix)
;;     (define-key map (kbd "h") #'codex-cabal-test-fix)

;;     (define-key map (kbd "t") #'codex-test-loop)
;;     (define-key map (kbd "s") #'codex-shell)
;;     map)
;;   "Codex keymap.")

;; (define-key global-map (kbd "C-c c") hc-codex-keymap)

(provide 'hc-codex)

;;; hc-codex.el ends here

