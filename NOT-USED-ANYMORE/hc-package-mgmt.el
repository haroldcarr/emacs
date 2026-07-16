;;; hc-package-mgmt.el --- Inspect and clean package.el installations -*- lexical-binding: t; -*-

;; This file provides conservative helpers for Emacs package.el.
;;
;; It is especially useful when packages come from both:
;;
;;   package-user-dir          ; normally ~/.emacs.d/elpa
;;   package-directory-list    ; e.g. read-only Nix store directories
;;
;; IMPORTANT:
;;
;; - Do not delete packages from /nix/store with package.el.
;;   Remove them from Home Manager/Nix configuration and rebuild.
;;
;; - `package-autoremove' removes packages no longer needed as dependencies.
;;   It is not specifically an "old version cleanup" command.
;;
;; - In `list-packages', "~" marks obsolete/older installed versions and
;;   "x" performs the marked operations.
;;
;; References:
;;
;; GNU Emacs manual: Packages
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
;;
;; GNU Emacs manual: Package Menu
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Menu.html
;;
;; Emacs Lisp manual: Packaging
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html
;;
;; GNU ELPA
;; https://elpa.gnu.org/
;;
;; NonGNU ELPA
;; https://elpa.nongnu.org/
;;
;; MELPA
;; https://melpa.org/
;;
;; use-package manual
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/
;;
;; Overview of package.el and other Emacs package approaches
;; https://zellyn.com/2024/08/emacs-packages/

;; ---------------------------------------------------------------------------
;; Suggested workflow
;;
;; 1. Inspect where packages can come from:
;;
;;      M-: package-user-dir
;;      M-: package-directory-list
;;
;; 2. If deletion reports a nonexistent directory:
;;
;;      M-x hc-package-refresh-installed-metadata
;;      M-x hc-package-alist-missing-directories
;;
;; 3. Find packages with multiple installed descriptors:
;;
;;      M-x hc-package-alist-duplicates
;;
;; 4. Inspect one duplicate in detail:
;;
;;      M-x hc-package-show-sources
;;
;;    Also check which copy Emacs would actually load:
;;
;;      M-x hc-package-locate-library
;;
;; 5. Delete one exact user-installed version:
;;
;;      M-x hc-package-delete-version
;;
;;    The command refuses to delete Nix-provided packages.
;;
;; 6. Standard package-menu method for old versions:
;;
;;      M-x list-packages
;;      ~    mark obsolete installed versions
;;      x    execute marked operations
;;
;; 7. Remove dependencies no longer required by selected packages:
;;
;;      M-x package-autoremove
;;
;;    This serves a different purpose from removing duplicate versions.
;;
;; Example:
;;
;; To delete MELPA which-key version 20240620.2145 while retaining 3.6.1:
;;
;;      M-x hc-package-delete-version
;;      Package: which-key
;;      Version: 20240620.2145

(require 'package)
(require 'seq)
(require 'subr-x)

(defun hc-package-refresh-installed-metadata ()
  "Rebuild `package-alist' from package directories on disk.

Run this when `package-alist' contains descriptors for package
directories that no longer exist.

This reads both `package-user-dir' and `package-directory-list'.
It does not download archive contents and does not delete anything."
  (interactive)
  (setq package-alist nil)
  (package-load-all-descriptors)
  (when package-quickstart
    (package-quickstart-refresh))
  (message "Reloaded %d installed package entries"
           (length package-alist)))

(defun hc-package-all-descriptors ()
  "Return all descriptors currently recorded in `package-alist'."
  (apply #'append (mapcar #'cdr package-alist)))

(defun hc-package-desc-version-string (desc)
  "Return DESC's version as a printable string."
  (package-version-join (package-desc-version desc)))

(defun hc-package-desc-location (desc)
  "Return DESC's directory, or a readable placeholder."
  (or (package-desc-dir desc) "<no directory>"))

(defun hc-package-user-installed-p (desc)
  "Return non-nil when DESC is installed under `package-user-dir'."
  (let ((dir (package-desc-dir desc)))
    (and dir
         (file-in-directory-p
          (file-truename dir)
          (file-truename package-user-dir)))))

(defun hc-package-nix-installed-p (desc)
  "Return non-nil when DESC resides in the Nix store."
  (let ((dir (package-desc-dir desc)))
    (and dir (string-prefix-p "/nix/store/" (file-truename dir)))))

(defun hc-package-show-sources (package)
  "Show every installed descriptor for PACKAGE.

Use this before deleting a duplicate so you can see its exact version,
archive, and filesystem location."
  (interactive
   (list
    (intern
     (completing-read
      "Package: "
      (mapcar (lambda (entry) (symbol-name (car entry))) package-alist)
      nil t))))
  (let ((descs (cdr (assq package package-alist))))
    (unless descs
      (user-error "Package not found: %s" package))
    (with-help-window "*Package Sources*"
      (princ (format "%s\n\n" package))
      (dolist (desc descs)
        (princ
         (format "version: %s\narchive: %s\ndir:     %s\nexists:  %s\n\n"
                 (hc-package-desc-version-string desc)
                 (or (package-desc-archive desc) "<unknown>")
                 (hc-package-desc-location desc)
                 (if (and (package-desc-dir desc)
                          (file-directory-p (package-desc-dir desc)))
                     "yes"
                   "NO")))))))

(defun hc-package-alist-duplicates ()
  "Display packages having multiple descriptors in `package-alist'.

This includes duplicates across `package-user-dir' and read-only package
directories such as Nix store paths.  Nothing is deleted."
  (interactive)
  (let ((duplicates
         (seq-filter (lambda (entry) (> (length (cdr entry)) 1))
                     package-alist)))
    (with-help-window "*Package Duplicates*"
      (if (null duplicates)
          (princ "No duplicate package descriptors found.\n")
        (dolist (entry
                 (sort duplicates
                       (lambda (a b)
                         (string-lessp (symbol-name (car a))
                                       (symbol-name (car b))))))
          (princ (format "%s\n" (car entry)))
          (dolist (desc (cdr entry))
            (princ
             (format "  %-18s  %-8s  %s%s\n"
                     (hc-package-desc-version-string desc)
                     (or (package-desc-archive desc) "?")
                     (hc-package-desc-location desc)
                     (if (and (package-desc-dir desc)
                              (not (file-directory-p
                                    (package-desc-dir desc))))
                         "  [MISSING]"
                       ""))))
          (princ "\n"))))))

(defun hc-package-alist-missing-directories ()
  "Display descriptors whose package directories do not exist.

If this reports entries, first run
`hc-package-refresh-installed-metadata'."
  (interactive)
  (let ((missing
         (seq-filter
          (lambda (desc)
            (let ((dir (package-desc-dir desc)))
              (and dir (not (file-directory-p dir)))))
          (hc-package-all-descriptors))))
    (with-help-window "*Missing Package Directories*"
      (if (null missing)
          (princ "No missing package directories found.\n")
        (dolist (desc missing)
          (princ
           (format "%-30s %-18s %s\n"
                   (package-desc-name desc)
                   (hc-package-desc-version-string desc)
                   (hc-package-desc-location desc))))))))

(defun hc-package-delete-version (package version)
  "Delete user-installed PACKAGE at VERSION.

PACKAGE is a symbol or string.  VERSION is a string such as \"3.6.1\" or
\"20240620.2145\".

This refuses to delete packages outside `package-user-dir', including
Nix store packages."
  (interactive
   (let* ((package
           (intern
            (completing-read
             "Package: "
             (mapcar (lambda (entry) (symbol-name (car entry)))
                     package-alist)
             nil t)))
          (descs (cdr (assq package package-alist)))
          (version
           (completing-read
            "Version: "
            (mapcar #'hc-package-desc-version-string descs)
            nil t)))
     (list package version)))
  (setq package (if (symbolp package) package (intern package)))
  (let* ((descs (cdr (assq package package-alist)))
         (desc
          (seq-find
           (lambda (candidate)
             (equal version
                    (hc-package-desc-version-string candidate)))
           descs)))
    (unless desc
      (user-error "No installed %s version %s" package version))
    (unless (package-desc-dir desc)
      (user-error "%s %s has no package directory" package version))
    (unless (file-directory-p (package-desc-dir desc))
      (user-error "Directory is missing; refresh metadata first: %s"
                  (package-desc-dir desc)))
    (unless (hc-package-user-installed-p desc)
      (user-error "Refusing to delete package outside package-user-dir: %s"
                  (package-desc-dir desc)))
    (when (yes-or-no-p
           (format "Delete %s %s from %s? "
                   package version (package-desc-dir desc)))
      (package-delete desc)
      (message "Deleted %s %s" package version))))

(defun hc-package-locate-library (package)
  "Show which library Emacs would load for PACKAGE.

For example, this reveals which duplicate `which-key' installation wins."
  (interactive
   (list
    (completing-read
     "Library/package: "
     (mapcar (lambda (entry) (symbol-name (car entry))) package-alist)
     nil t)))
  (message "%s" (or (locate-library package)
                    (format "Library not found: %s" package))))

(provide 'hc-package-mgmt)
;;; hc-package-mgmt.el ends here
