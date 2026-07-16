;;; hc-package-cleanup.el --- Safely clean duplicate package versions -*- lexical-binding: t; -*-

;; Conservative replacement for hc-package-mgmt.el and hc-package-migrate.el.
;;
;; Workflow:
;;   M-x hc-package-cleanup-audit
;;   M-x hc-package-cleanup-one
;; or
;;   M-x hc-package-cleanup-all
;;
;; Never force-deletes
;;
;; It will not delete:
;; - the newest installed version
;; - a version currently loaded
;; - packages outside package-user-dir
;; - dependency-protected versions
;;
;; Documentation:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Menu.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Package-Archives.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Package-Installation.html
;;
;; ALTERNATIVE
;; list-packages
;; ~
;; x
;;
;; The normal package.el workflow for deleting obsolete versions.
;;
;; This custom tool only adds extra safeguards and reporting:
;; - shows which version is currently loaded
;; - refuses to delete loaded versions
;; - refuses to touch /nix/store packages
;; - handles stale/missing directories more explicitly
;; - lets you clean one package at a time

(require 'package)
(require 'seq)
(require 'subr-x)

(defun hc-package-cleanup--version-string (desc)
  "Return DESC's version as a string."
  (package-version-join (package-desc-version desc)))

(defun hc-package-cleanup--directory (desc)
  "Return DESC's canonical package directory."
  (when-let ((dir (package-desc-dir desc)))
    (file-name-as-directory (file-truename dir))))

(defun hc-package-cleanup--user-package-p (desc)
  "Return non-nil when DESC is below `package-user-dir'."
  (when-let ((dir (hc-package-cleanup--directory desc)))
    (file-in-directory-p
     dir
     (file-name-as-directory (file-truename package-user-dir)))))

(defun hc-package-cleanup--existing-p (desc)
  "Return non-nil when DESC's directory exists."
  (when-let ((dir (package-desc-dir desc)))
    (file-directory-p dir)))

(defun hc-package-cleanup--loaded-files (desc)
  "Return loaded Lisp files belonging to DESC."
  (when-let ((dir (hc-package-cleanup--directory desc)))
    (seq-filter
     (lambda (file)
       (and (stringp file)
            (file-name-absolute-p file)
            (file-exists-p file)
            (file-in-directory-p (file-truename file) dir)))
     (mapcar #'car load-history))))

(defun hc-package-cleanup--loaded-p (desc)
  "Return non-nil when files from DESC are loaded."
  (and (hc-package-cleanup--loaded-files desc) t))

(defun hc-package-cleanup--newer-p (a b)
  "Return non-nil when descriptor A is newer than B."
  (version-list-<
   (package-desc-version b)
   (package-desc-version a)))

(defun hc-package-cleanup--sorted-descs (package)
  "Return installed descriptors for PACKAGE, newest first."
  (sort (copy-sequence (alist-get package package-alist))
        #'hc-package-cleanup--newer-p))

(defun hc-package-cleanup--duplicates ()
  "Return package symbols having multiple installed descriptors."
  (seq-filter
   (lambda (package)
     (> (length (alist-get package package-alist)) 1))
   (mapcar #'car package-alist)))

(defun hc-package-cleanup--status (desc newest)
  "Return a status string for DESC relative to NEWEST."
  (string-join
   (delq nil
         (list
          (when (eq desc newest) "NEWEST")
          (when (hc-package-cleanup--loaded-p desc) "LOADED")
          (unless (hc-package-cleanup--existing-p desc) "MISSING")
          (unless (hc-package-cleanup--user-package-p desc) "EXTERNAL")))
   ","))

(defun hc-package-cleanup--print-package (package)
  "Print duplicate-version information for PACKAGE."
  (let* ((descs (hc-package-cleanup--sorted-descs package))
         (newest (car descs)))
    (princ (format "%s\n" package))
    (dolist (desc descs)
      (princ
       (format "  %-20s %-24s %s\n"
               (hc-package-cleanup--version-string desc)
               (hc-package-cleanup--status desc newest)
               (or (package-desc-dir desc) "<no directory>"))))
    (princ "\n")))

(defun hc-package-cleanup-audit ()
  "Display every package having multiple installed versions.

Labels are NEWEST, LOADED, MISSING, and EXTERNAL.
This command never changes anything."
  (interactive)
  (with-help-window "*Package Cleanup Audit*"
    (let ((packages
           (sort (hc-package-cleanup--duplicates)
                 (lambda (a b)
                   (string-lessp (symbol-name a)
                                 (symbol-name b))))))
      (if packages
          (dolist (package packages)
            (hc-package-cleanup--print-package package))
        (princ "No packages have multiple installed versions.\n")))))

(defun hc-package-cleanup--deletable-descs (package)
  "Return conservatively deletable descriptors for PACKAGE."
  (let* ((descs (hc-package-cleanup--sorted-descs package))
         (newest (car descs)))
    (seq-filter
     (lambda (desc)
       (and (not (eq desc newest))
            (hc-package-cleanup--existing-p desc)
            (hc-package-cleanup--user-package-p desc)
            (not (hc-package-cleanup--loaded-p desc))))
     descs)))

(defun hc-package-cleanup--read-package ()
  "Read a package having multiple installed versions."
  (intern
   (completing-read
    "Package with duplicate versions: "
    (mapcar #'symbol-name
            (sort (hc-package-cleanup--duplicates)
                  (lambda (a b)
                    (string-lessp (symbol-name a)
                                  (symbol-name b)))))
    nil t)))

(defun hc-package-cleanup--read-desc (package)
  "Read a conservatively deletable descriptor for PACKAGE."
  (let ((descs (hc-package-cleanup--deletable-descs package)))
    (unless descs
      (user-error
       "No safely deletable versions of %s; restart Emacs or inspect the audit"
       package))
    (let* ((versions (mapcar #'hc-package-cleanup--version-string descs))
           (version
            (completing-read
             (format "Delete old %s version: " package)
             versions nil t)))
      (seq-find
       (lambda (desc)
         (equal version (hc-package-cleanup--version-string desc)))
       descs))))

(defun hc-package-cleanup-delete-version (package desc)
  "Delete PACKAGE descriptor DESC after conservative safety checks.

This never passes FORCE to `package-delete'."
  (unless (memq desc (alist-get package package-alist))
    (user-error "Descriptor is no longer installed for %s" package))
  (let* ((descs (hc-package-cleanup--sorted-descs package))
         (newest (car descs)))
    (when (eq desc newest)
      (user-error "Refusing to delete newest installed %s" package))
    (unless (hc-package-cleanup--existing-p desc)
      (user-error "Package directory is missing: %s"
                  (package-desc-dir desc)))
    (unless (hc-package-cleanup--user-package-p desc)
      (user-error "Refusing to delete package outside package-user-dir: %s"
                  (package-desc-dir desc)))
    (when (hc-package-cleanup--loaded-p desc)
      (user-error "Refusing to delete loaded package version: %s"
                  (package-desc-dir desc)))
    (when
        (yes-or-no-p
         (format "Delete %s %s from %s? "
                 package
                 (hc-package-cleanup--version-string desc)
                 (package-desc-dir desc)))
      (condition-case err
          (progn
            (package-delete desc)
            (message "Deleted %s %s"
                     package
                     (hc-package-cleanup--version-string desc))
            t)
        (error
         (message "Did not delete %s %s: %s"
                  package
                  (hc-package-cleanup--version-string desc)
                  (error-message-string err))
         nil)))))

(defun hc-package-cleanup-one (package)
  "Interactively delete old versions of one PACKAGE."
  (interactive (list (hc-package-cleanup--read-package)))
  (let ((continue t))
    (while continue
      (let ((descs (hc-package-cleanup--deletable-descs package)))
        (if (null descs)
            (progn
              (message "No more safely deletable versions of %s" package)
              (setq continue nil))
          (let ((desc (hc-package-cleanup--read-desc package)))
            (hc-package-cleanup-delete-version package desc)
            (setq continue
                  (and (hc-package-cleanup--deletable-descs package)
                       (yes-or-no-p
                        (format "Delete another old version of %s? "
                                package))))))))))

(defun hc-package-cleanup-all ()
  "Review every package having duplicate installed versions.

Errors are reported and processing continues with the next package."
  (interactive)
  (let ((debug-on-error nil)
        (debug-on-signal nil)
        (packages
         (sort (copy-sequence (hc-package-cleanup--duplicates))
               (lambda (a b)
                 (string-lessp (symbol-name a)
                               (symbol-name b))))))
    (dolist (package packages)
      (when (yes-or-no-p (format "Clean old versions of %s? " package))
        (condition-case err
            (hc-package-cleanup-one package)
          (quit
           (message "Skipped %s" package))
          (error
           (message "Skipped %s after error: %s"
                    package
                    (error-message-string err))))))
    (message "Package cleanup review finished")))

(defun hc-package-cleanup-refresh-metadata ()
  "Rebuild `package-alist' from package directories on disk.

Use this only when the audit shows MISSING entries or package.el tries
removing directories which no longer exist."
  (interactive)
  (setq package-alist nil)
  (package-load-all-descriptors)
  (when package-quickstart
    (package-quickstart-refresh))
  (message "Reloaded installed package metadata"))

(provide 'hc-package-cleanup)
;;; hc-package-cleanup.el ends here
