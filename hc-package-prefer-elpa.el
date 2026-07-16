;;; hc-package-prefer-elpa.el --- xxx          -*- lexical-binding: t; -*-

(defun hc-install-from-melpa (package)
  "PACKAGE the one to install."
  (package-install
   (seq-find (lambda (desc) (equal (package-desc-archive desc) "melpa"))
             (alist-get package package-archive-contents))))

;; ------------------------------------------------------------------------------

;; For forcing install of things in package-pinned-packages.
;;
;; M-x package-refresh-contents
;; M-x hc-package-install-pinned-versions

;; This processes your entire =package-pinned-packages= list, installing the GNU or
;; NonGNU version even when a numerically newer MELPA snapshot is already
;; installed.

;; Afterward:

;; 1. Restart Emacs.
;; 2. Run =M-x list-packages=.
;; 3. Press =~= to mark obsolete versions.
;; 4. Press =x= to delete them.

;; =haskell-mode= may fail again because of its separate NonGNU 17.5
;; byte-compilation problem. The function will report that failure and continue
;; with the remaining packages.

(require 'package)
(require 'seq)

(defun hc-package-install-pinned-versions ()
  "Install each pinned package from its pinned archive.
Continue with the next package when one installation fails."
  (interactive)
  (dolist (pin package-pinned-packages)
    (let* ((package (car pin))
           (archive (cdr pin))
           (desc (seq-find (lambda (candidate) (equal (package-desc-archive candidate) archive))
                           (alist-get package package-archive-contents))))
      (cond
       ((null desc)
        (message "SKIP %s: not found in %s" package archive))

       ((seq-some
         (lambda (installed) (equal (package-desc-version installed)
                                    (package-desc-version desc)))
         (alist-get package package-alist))
        (message "OK %s: pinned %s version already installed" package archive))

       (t
        (condition-case err
            (progn
              (message "Installing %s %s from %s..."
                       package
                       (package-version-join (package-desc-version desc))
                       archive)
              (package-install desc))
          (error (message "FAILED %s from %s: %s"
                          package archive
                          (error-message-string err))))))))
  (message "DONE: hc-package-install-pinned-versions"))

;; ------------------------------------------------------------------------------

;; This is to delete MELPA versions that have ELPA versions.
;; 1. Restart Emacs.
;; 2. Run =M-x hc-delete-old-melpa-versions=.
;; 3. Restart Emacs again.
;; 4. Run =M-x list-packages=.

(require 'package)

(defconst hc-melpa-versions-to-delete
  '((evil             . "20251108.138")
    (flycheck         . "20260320.1715")
    (helm             . "20260712.427")
    (helm-core        . "20260618.1908")
    (hydra            . "20250316.1254")
    (idris-mode       . "20260506.2033")
    (js2-mode         . "20260627.1342")
    (llama            . "20260601.1455")
    (lv               . "20200507.1518")
    (magit            . "20260711.1257")
    (magit-section    . "20260709.950")
    (markdown-mode    . "20260425.954")
    (mmm-mode         . "20240222.428")
    (modus-themes     . "20260714.716")
    (multiple-cursors . "20260419.931")
    (orderless        . "20260519.1029")
    (popup            . "20251231.1622")
    (posframe         . "20260527.857")
    (projectile       . "20260713.1548")
    (prop-menu        . "20150728.1118")
    (request          . "20250219.2213")
    (tablist          . "20260623.1855")
    (transient        . "20260701.1255")
    (treepy           . "20260531.1144")
    (wfnames          . "20260706.903")
    (with-editor      . "20260701.1252")
    (yaml             . "20260605.834")
    (yaml-mode        . "20260420.156")
    (yasnippet        . "20250602.1342")))

(defun hc-delete-old-melpa-versions ()
  "Delete the specified MELPA versions, one at a time with confirmation."
  (interactive)
  (dolist (item hc-melpa-versions-to-delete)
    (let* ((package (car item))
           (version (cdr item))
           (desc (seq-find (lambda (candidate)
                             (equal (package-version-join (package-desc-version candidate))
                                    version))
                           (alist-get package package-alist))))
      (when (and desc (yes-or-no-p (format "Delete MELPA %s %s? " package version)))
        (condition-case err (package-delete desc t)
          (error (message "Could not delete %s %s: %s"
                          package version
                          (error-message-string err)))))))
  (message "DONE: hc-delete-old-melpa-versions"))

(provide 'hc-package-prefer-elpa)

;;; hc-package-prefer-elpa.el ends here
