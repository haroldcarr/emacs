;;; hc-ai --- Summary                      -*- lexical-binding: t; -*-

;;; Commentary:

;; https://github.com/karthink/gptel

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2026 Apr 10 (Fri) 17:07:49 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

(require 'url)

;; gptel requires Transient 0.7.4 or higher.
;; Transient is a built-in package and Emacs does not update it by default.
;; The following enables updating the built-in package:
(setq package-install-upgrade-built-in t)

(defun hc-install-gptel-from-github ()
  "Download, extract, and install gptel from GitHub master zip."
  (interactive)
  (let* ((url "https://github.com/karthink/gptel/archive/refs/heads/master.zip")
         (zip "/tmp/gptel-master.zip")
         (dir "/tmp/gptel-master"))
    ;; 1. Download
    (message "Downloading %s..." url)
    (url-copy-file url zip t)
    ;; 2. Extract (macOS/Linux unzip)
    (message "Extracting to %s..." dir)
    (when (file-exists-p dir) (delete-directory dir t))
    (call-process "unzip" nil nil nil zip "-d" "/tmp/") ;; assumes unzip in PATH
    ;; 3. Install via package-install-file
    (message "Installing gptel...")
    (package-install-file dir)
    (message "Done. Restart Emacs or (require 'gptel)")))

;; - ~M-x hc-install-gptel-from-github~

;; - Installs to =~/.emacs.d/elpa/gptel-<date>/=
;; - Subsequent ~M-x package-refresh-contents~ may override with MELPA version
;; - To pin to this version: add to ~package-pinned-packages~ : ~'(gptel . "manual")~

;; Verification:
;; - ~M-x describe-package RET gptel RET~ should show installed
;; - ~M-x gptel! : mode line should show ~[Ollama:qwen2.5-coder:14b]~

(require 'gptel)

(gptel-make-ollama "local-qwen2.5-coder:14b"
  :host "localhost:11434"
  :models '("qwen2.5-coder:14b")
)
(gptel-make-ollama "local-qwen2.5:14b"
  :host "localhost:11434"
  :models '("qwen2.5:14b")
)
;(setq gptel-model "qwen2.5-coder:14b")
(setq gptel-default-mode 'org-mode)
;(setq gptel-stream t)

;; - failing:
;;   - Check gptel version: `M-x package-describe-package RET gptel`
;;   - Update if < 7.0: `M-x package-refresh-contents` then `M-x package-install RET gptel`
;; - Fallback test:
;;   - `curl -X POST http://localhost:11434/api/generate -d '{"model":"qwen2.5-coder:14b","prompt":"hi"}'`
;;   - If curl works but Emacs fails → gptel config issue, not Ollama

;;(require 'gptel)
;; say hello to harold using my_hello_world tool. Do not worry about the specific form of the greeting.  Just call the tool and show me what it returns.
(gptel-make-tool
 :name "my_hello_world"
 :function (lambda (x) (message "hellO WoRlD %s" x) (concat "heLLo woRld: " x))
 :category "hello"
 :args '((:name "x"
          :type string
          :description "the person to say hello to"))
 :description "When I ask you to say hello, call the function with the name given.")

;;(setq gptel-tools "my_hello_world")

(provide 'hc-ai)

;;; hc-ai.el ends here
