;;; hc-windows-os --- windows

;;; Commentary:

;;; Code:

;; TODO: factor the following two together.

(defmacro hcRedefProc (procedure args &rest body)
  "PROCEDURE ARGS BODY."
  (let ((original-name
	 (intern (concat "*hc-emacs-original-" (format "%s" procedure) "*"))))
    `(progn
       (defvar ,original-name nil)
       (setq ,original-name (symbol-function ',procedure))
       (defun ,procedure ,args ,@body))))

(defmacro hcRedefVar (variable value)
  "VARIABLE VALUE."
  (let ((original-name
	 (intern (concat "*hc-emacs-original-" (format "%s" variable) "*"))))
    `(progn
       (defvar ,original-name nil)
       (setq ,original-name ,variable)
       (setq ,variable ,value))))

;; Do this early since it is used during init.
(hcRedefProc user-full-name () "."
	 "Harold Carr")

;; Change canonical into win32 (i.e., colon) so emacs can do dir tracking.
;; Same as hcMakeDriveColon script, but do not want to go to shell each time.
(defun hcMakeDriveColon-el (str) "STR."
  (if (and (>= (length str) 4)
	   (string-equal (substring str 0 2) "//")
	   (string-equal (substring str 3 4) "/"))
      (concat (substring str 2 3) ":" (substring str 3 (length str)))
    str))

(hcRedefProc file-name-absolute-p (str)
	 (funcall *hc-emacs-original-file-name-absolute-p*
		  (hcMakeDriveColon-el str)))

;(hcRedefProc expand-file-name (file &optional default)
;	 (funcall *hc-emacs-original-expand-file-name*
;		  (hcMakeDriveColon-el file) default))

(defvar file-name-buffer-file-type-alist)
(hcRedefVar file-name-buffer-file-type-alist '(("*.*" . t) (".*" . t)))

(defvar *hc-emacs-original-file-name-buffer-file-type-alist*)
(defun hcToggleBinaryModeMap () "."
  (interactive)
  (let ((tmp *hc-emacs-original-file-name-buffer-file-type-alist*))
    (setq *hc-emacs-original-file-name-buffer-file-type-alist*
	  file-name-buffer-file-type-alist)
    (setq file-name-buffer-file-type-alist
	  tmp)))

(provide 'hc-windows-os)

;;; hc-windows-os.el ends here
