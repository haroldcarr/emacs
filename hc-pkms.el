;;; hc-pkms --- Personal Knowledge Management System(s)            -*- lexical-binding: t; -*-

;;; Commentary:

;; ...

;;;;
;;;; Created       : 2026 Jul 08 (Wed) 14:56:06 by Harold Carr.
;;;; Last Modified : 2026 Jul 16 (Thu) 11:19:08 by Harold Carr.
;;;;

;;; Code:

(eval-when-compile (require 'use-package))

;; https://www.chiply.dev/post-hyperbole-hywiki  : Charlie Holland
;; https://rswgnu-hyperbole.mintlify.app/configuration/settings

;; HyWiki has no official delete command for a HyWikiWord.
;; To DELETE
;; 1. exit emacs
;; 2. emacs -Q `hcLocation fsync`/0-HYWIKI/.hywiki.eld
;; 3. For a page HyWikiWord
;;    - Delete its associated file
;;    - Remove its entry from .hywiki.eld
;;    For an Elisp HyWikiWord
;;    - remove its database entry
;; 4. exit emacs-Q
;; 5. start emacs
;; 6. run hc-hywiki-scan-then-update-dot-hywiki-dot-eld

(declare-function hyperbole-mode            "hyperbole")
(declare-function hywiki-mode               "hywiki")
(declare-function hywiki-get-referent-hasht "hywiki")
(declare-function hywiki-cache-save         "hywiki")

(use-package hyperbole
  :custom
  (hywiki-directory (concat (hcFsync) "/0-HYWIKI/"))
  ;; t        = Hyperbole M-RET handles more Org contexts
  ;; 'buttons = Hyperbole M-RET mainly handles buttons/links
  ;; nil      = leave Org M-RET alone
  (hsys-org-enable-smart-keys t)
  :config
  (hyperbole-mode 1) ;; C-h h h m a ;; HyWikiWords active everywhere
  (require 'hywiki)
  (hywiki-mode :all) ;; C-h h h o a ;; M-RET does Hyperbole actions in Org mode
)

(defun hc-hywiki-scan-then-update-dot-hywiki-dot-eld ()
  (interactive)
  (setq hywiki--directory-mod-time nil)
  ;; if hywiki-directory changed it rebuilds hywiki--referent-hasht (the in-memory database)
  ;; from current page files while preserving non-page referents.
  (hywiki-get-referent-hasht)
  ;; Writes that hash table to .hywiki.eld
  ;; and updates the modification time and checksum.
  (hywiki-cache-save))

(provide 'hc-pkms)

;;; hc-pkms.el ends here
