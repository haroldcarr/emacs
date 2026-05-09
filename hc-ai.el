;;; hc-ai --- Summary                      -*- lexical-binding: t; -*-

;;; Commentary:

;; https://github.com/karthink/gptel

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2026 May 08 (Fri) 12:23:05 by Harold Carr.
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

;; ~M-x hc-install-gptel-from-github~

;; Installs to =~/.emacs.d/elpa/gptel-<date>/=
;; Subsequent ~M-x package-refresh-contents~ may override with MELPA version
;; To pin to this version: add to ~package-pinned-packages~ : ~'(gptel . "manual")~

;; Verification:
;; ~M-x describe-package RET gptel RET~ should show installed
;; ~M-x gptel! : mode line should show ~[Ollama:qwen2.5-coder:14b]~

(require 'gptel)

(defmacro hc-gptel-make-ollama (prefix model-name &rest args)
  `(gptel-make-ollama ,(concat prefix "-" model-name)
     :host "localhost:11434"
     :models '(,model-name)
     ,@args))

(hc-gptel-make-ollama "cloud" "deepseek-v4-pro:cloud")
(hc-gptel-make-ollama "cloud" "minimax-m2.7:cloud")

(hc-gptel-make-ollama "local" "gemma4:31b")
(hc-gptel-make-ollama "local" "gemma4:31b-coding-mtp-bf16")
(hc-gptel-make-ollama "local" "qwen3-coder:30b")
(hc-gptel-make-ollama "local" "qwen3.5:35b")
(hc-gptel-make-ollama "local" "qwen3.6:35b")

(gptel-make-openai "OpenAI"
  :key "PASTE KEY HERE" ;; TODO : get from .authinfo
  :models '("gpt-4o"))

(gptel-make-openai "llama.cpp"
  :protocol "http"
  :host "localhost:8080"
  :stream t
  :key "dummy"
  :models '(llama.cpp))

(setq gptel-default-mode 'org-mode)
(setq gptel-stream t)

;; if gptel fails:
;; - Check gptel version: `M-x package-describe-package RET gptel`
;; - Update if < 7.0: `M-x package-refresh-contents` then `M-x package-install RET gptel`
;; Fallback test:
;; - curl -X POST http://localhost:11434/api/generate -d '{"model":"qwen2.5-coder:14b","prompt":"hi"}'
;; If curl works but Emacs fails → gptel config issue, not Ollama
;; say hello to John Doe using my_hello_world tool.
;; Do not worry about the specific form of the greeting.
;; Just call the tool and show me what it returns.

(gptel-make-tool
 :name "my_hello_world"
 :function (lambda (x) (message "hellO WoRlD %s" x) (concat "heLLo woRld: " x))
 :category "hello"
 :args '((:name "x"
          :type string
          :description "the person to say hello to"))
 :description "When I ask you to say hello, call the function with the name given.")

;;; TOOLS

(require 'cl-lib)

(defun git-clone-url-in-dir (url target-directory)
  "Clones the GIT repository located at URL into TARGET-DIRECTORY."
  (interactive "sGit URL: \nsTarget Directory: ")
  (unless (file-exists-p target-directory)
    (error "Directory %s does not exist." target-directory))
  (let ((default-directory target-directory))
    (with-temp-buffer
      (if (zerop (call-process "git" nil t nil "clone" url))
          (message "Git repository cloned successfully.")
        (error "git clone failed")))))

;; (git-clone-url-in-dir "git@github.com:karthink/gptel-agent.git" (hcLocation 'emacs))
;; add gptel-agent to .gitignore
;; M-x package-install-file <on the gptel-agent repo directory>
(require 'gptel-agent)

(provide 'hc-ai)

;;; hc-ai.el ends here
