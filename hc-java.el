;;; hc-java.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar *hcJavaMode* 'not-google)
(declare-function  google-set-c-style ".")
(add-hook 'java-mode-hook
  (lambda () (if (eq *hcJavaMode* 'google) (google-set-c-style))))

;; M-x google-set-c-style
(use-package google-c-style)

;; Make java mode support Java 1.5 annotations.
(declare-function auto-complete-mode ".")
(use-package java-mode-indent-annotations
  :config
  (add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
  (add-hook 'java-mode-hook (lambda () (auto-complete-mode 1))))

(defvar *compile-threshold*)
(setq *compile-threshold* " -XX:CompileThreshold=2 ")

(defun HC-JAVA_HOME () "."
  (cond ((getenv "JAVA_HOME"))
	(t (error "No default JDK"))))
(defun HC-JAVA_HOME-bin     () "." (concat (HC-JAVA_HOME) "/bin"))
(defun HC-JAVA_HOME-classes () "." (concat (HC-JAVA_HOME) "/jre/lib/rt.jar"))

(use-package hc-lsp-dap-java)

(provide 'hc-java)

;;; hc-java.el ends here
