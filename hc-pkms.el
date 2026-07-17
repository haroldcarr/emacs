;;; hc-pkms --- Personal Knowledge Management System(s)            -*- lexical-binding: t; -*-

;;; Commentary:

;; ...

;;;;
;;;; Created       : 2026 Jul 08 (Wed) 14:56:06 by Harold Carr.
;;;; Last Modified : 2026 Jul 16 (Thu) 13:06:16 by Harold Carr.
;;;;

;;; Code:

;; https://www.chiply.dev/post-hyperbole-hywiki  : Charlie Holland
;; https://rswgnu-hyperbole.mintlify.app/configuration/settings

;; M-RET     : Act on HyWikiWord; creates word/page if does not exist
;; M-0 M-RET : Elisp RefType

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

(eval-when-compile (require 'use-package))
(declare-function hash-make                  "hact")
(declare-function hash-map                   "hact")
(declare-function hyperbole-mode             "hyperbole")
(declare-function hywiki-cache-save          "hywiki")
(declare-function hywiki-get-referent-hasht  "hywiki")
(declare-function hywiki-make-referent-hasht "hywiki")
(declare-function hywiki-mode                "hywiki")
(declare-function hywiki-non-page-elt        "hywiki")

(defun hc-hywiki-add-title (result)
  "Add a title to a newly created HyWiki page."
  (when-let ((file (cdr-safe result)))
    (when (and (file-exists-p file)
               (= (file-attribute-size (file-attributes file)) 0))
      (with-temp-file file
        (insert "#+TITLE: " (file-name-base file) "\n")
        (insert "# ------------------------------------------------------------------------------\n\n"))))
  result)

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

  (advice-add 'hywiki-add-page :filter-return #'hc-hywiki-add-title)
)

(provide 'hc-pkms)

;;; hc-pkms.el ends here
