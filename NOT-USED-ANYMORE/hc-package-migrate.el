;;; hc-package-migrate.el --- Assist migration from MELPA to ELPA -*- lexical-binding: t; -*-

;; Run in this order:
;;
;; M-x package-refresh-contents
;; M-x hc-package-migration-audit
;; M-x hc-package-migrate-one
;;
;; Or process the audit list interactively:
;;
;; M-x hc-package-migrate-all
;;
;; Notes:
;;
;; - Installation failures are reported and skipped.
;; - Deletion failures are reported and skipped.
;; - Package versions are never force-deleted.
;; - Restart Emacs after migrations before judging which version loads.

(require 'package)
(require 'seq)

(defun hc-package-archive-descs (package archive)
  "Return PACKAGE descriptors from ARCHIVE."
  (seq-filter
   (lambda (desc)
     (equal (package-desc-archive desc) archive))
   (alist-get package package-archive-contents)))

(defun hc-package-newest-desc (package archive)
  "Return newest PACKAGE descriptor from ARCHIVE."
  (car
   (sort (copy-sequence
          (hc-package-archive-descs package archive))
         (lambda (a b)
           (version-list-<
            (package-desc-version b)
            (package-desc-version a))))))

(defun hc-package-target-desc (package)
  "Return PACKAGE from GNU ELPA, otherwise NonGNU ELPA."
  (or (hc-package-newest-desc package "gnu")
      (hc-package-newest-desc package "nongnu")))

(defun hc-package-installed-descs (package)
  "Return installed descriptors for PACKAGE."
  (alist-get package package-alist))

(defun hc-package-installed-version-p (package version)
  "Return non-nil if PACKAGE VERSION is installed."
  (seq-some
   (lambda (desc)
     (equal (package-desc-version desc) version))
   (hc-package-installed-descs package)))

(defun hc-package-melpa-installed-desc (package)
  "Return installed PACKAGE descriptor matching current MELPA version."
  (let ((melpa (hc-package-newest-desc package "melpa")))
    (when melpa
      (seq-find
       (lambda (installed)
         (equal (package-desc-version installed)
                (package-desc-version melpa)))
       (hc-package-installed-descs package)))))

(defun hc-package-migration-candidates ()
  "Return installed MELPA packages also available from ELPA."
  (seq-filter
   (lambda (package)
     (and (hc-package-melpa-installed-desc package)
          (hc-package-target-desc package)))
   (mapcar #'car package-alist)))

(defun hc-package-version-string (desc)
  "Return printable version of package DESC."
  (package-version-join
   (package-desc-version desc)))

;; Stage 1: read-only audit.

(defun hc-package-migration-audit ()
  "Show installed MELPA packages also available from ELPA."
  (interactive)
  (with-help-window "*Package Migration Audit*"
    (let ((packages
           (sort (hc-package-migration-candidates)
                 (lambda (a b)
                   (string-lessp
                    (symbol-name a)
                    (symbol-name b))))))
      (if (null packages)
          (princ "No migration candidates found.\n")
        (dolist (package packages)
          (let ((melpa  (hc-package-melpa-installed-desc package))
                (target (hc-package-target-desc package)))
            (princ
             (format "%-25s MELPA %-16s -> %-6s %s\n"
                     package
                     (hc-package-version-string melpa)
                     (package-desc-archive target)
                     (hc-package-version-string target)))))))))

;; Stage 2: migrate one package safely.

(defun hc-package-migrate-one (package)
  "Assist migration of PACKAGE from MELPA to GNU or NonGNU ELPA.

Installation and deletion errors are reported without aborting an
outer migration loop.  Old versions are never force-deleted."
  (interactive
   (list
    (intern
     (completing-read
      "Migrate package: "
      (mapcar #'symbol-name
              (hc-package-migration-candidates))
      nil t))))
  (let* ((melpa  (hc-package-melpa-installed-desc package))
         (target (hc-package-target-desc package))
         (target-version
          (and target (package-desc-version target))))
    (unless melpa
      (user-error "No matching MELPA installation found for %s"
                  package))
    (unless target
      (user-error "%s is not available from GNU or NonGNU ELPA"
                  package))

    (message "Installing %s %s from %s..."
             package
             (hc-package-version-string target)
             (package-desc-archive target))

    (condition-case err
        (progn
          (package-install target)

          (unless
              (hc-package-installed-version-p
               package target-version)
            (error "Installation could not be verified"))

          (message "Installed %s %s from %s"
                   package
                   (hc-package-version-string target)
                   (package-desc-archive target))

          (when
              (yes-or-no-p
               (format "Delete old MELPA %s %s? "
                       package
                       (hc-package-version-string melpa)))
            (condition-case delete-error
                (progn
                  ;; No FORCE argument.  If another package still
                  ;; requires this descriptor, package.el refuses.
                  (package-delete melpa)
                  (message "Deleted MELPA %s %s"
                           package
                           (hc-package-version-string melpa)))
              (error
               (message "Installed replacement, but did not delete old version: %s"
                        (error-message-string delete-error)))))

          t)
      (error
       (message "Migration of %s failed: %s"
                package
                (error-message-string err))
       nil))))

;; Stage 3: offer each candidate, continuing after errors.

;; It catches each package's failure at the outer loop and continues.

(defun hc-package-migrate-all ()
  "Offer to migrate each candidate, continuing after all errors."
  (interactive)
  (let ((debug-on-error nil)
        (debug-on-signal nil)
        (packages
         (sort (copy-sequence (hc-package-migration-candidates))
               (lambda (a b)
                 (string-lessp (symbol-name a)
                               (symbol-name b))))))
    (dolist (package packages)
      (when (yes-or-no-p (format "Migrate %s? " package))
        (condition-case err
            (hc-package-migrate-one package)
          (error
           (message "Skipping %s after error: %s"
                    package
                    (error-message-string err))))))
    (message "Package migration assistant finished")))

(provide 'hc-package-migrate)
;;; hc-package-migrate.el ends here
