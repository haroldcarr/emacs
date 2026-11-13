;;; Commentary:

;;; Code:

;; -----------------------------------------------------------------------------
(when (version<= "30.2" emacs-version)
  (require 'casual)
  (casual-init))
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-remoto) ;; browse github repos without cloning
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-ai)
;; ------------------------------------------------------------------------------
;;TODO(hcSectionLoad hc-spelling)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-time)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-music-emms)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-youtube)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-reaper)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-image)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-sync)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-pinboard-pick)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-browser-history)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-theme-switch)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-finance)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-markdown)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-pdf)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-gnus)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-bookmarks)
;; ------------------------------------------------------------------------------
;;TODO(hcSectionLoad hc-web-browsing)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-timestamp)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-org-mode)
;; ------------------------------------------------------------------------------
;;TODO(hcSectionLoad hc-pkms) ;; must come after org-mode
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-tags)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-git)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-sdedit)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-dir-tree)
;; ------------------------------------------------------------------------------
;; move/copy between two dired windows
(defvar dired-dwim-target)
(setq dired-dwim-target t)
;;(use-package hc-dired)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-epub)
;; ------------------------------------------------------------------------------
;;(hcSectionLoad hc-greek)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-icr-mini-buffer-and-in-buffer-incremental-completing-read)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-yasnippet)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-daemon-emacsclient)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-projectile)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-compilation)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-agda)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-haskell)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-lean)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-lua)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-python)
;; ------------------------------------------------------------------------------
(hcSectionLoadOnDevMachines hc-rust)
;; ------------------------------------------------------------------------------
;; * Typescript
;; (when (member (hcMachineName) hc-dev-machines)
;;   (hcSection "Typescript")
;;   (use-package typescript)
;;   (setq typescript-indent-level 2))
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-java)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-lisps)
;; ------------------------------------------------------------------------------
;; This should come AFTER all language-specific setup above.
(when (member (hcMachineName) hc-dev-machines)
  (hcSection "LSP and EGLOT")
  ;;(use-package hc-lsp-pick)
  (use-package hc-lsp)
  (use-package hc-eglot)
)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-calendar)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-alignment)
;; ------------------------------------------------------------------------------
(hcSectionLoad hc-google)
;; ------------------------------------------------------------------------------
;; * Misc

(hcSection "Misc")
(use-package httpcode :defer t)

(defun hc-tps (ntx ndays nhours nminutes nseconds)
  "NTX NDAYS NHOURS NMINUTES NSECONDS."
  (let ((days-seconds    (* (* (* ndays 24) 60) 60))
        (hours-seconds   (* (* nhours       60) 60))
        (minutes-seconds (* nminutes            60)))
    (/ ntx (+ days-seconds hours-seconds minutes-seconds nseconds))))

;; https://emacs.stackexchange.com/a/73152
;; (hc-reverse-dependencies 'transient t)
(defun hc-reverse-dependencies (package-name resolve-deps-recursively)
  "PACKAGE-NAME, RESOLVE-DEPS-RECURSIVELY."
  (let ((needle package-name)
        curr-deps
        all-rdeps)
    (dolist (curr package-archive-contents all-rdeps)
      ;; (car curr) => pkg
      ;; (cadr curr) => pkg-desc
      (setq curr-deps (if resolve-deps-recursively
                          (package--dependencies (car curr))
                        (mapcar #'car (package-desc-reqs (cadr curr)))))
      (when (memq needle curr-deps)
        (push (car curr) all-rdeps)))))

;; ------------------------------------------------------------------------------
(hcSectionLoad hc-appearance)
;; ------------------------------------------------------------------------------
(cond ((string-equal (hcMachineName) "hc2025") (hc-appearance-startup)))
