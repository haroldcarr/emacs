;;; hc-ssh.el --- Support for remote logins using ssh.

;; Copyright (C) 1996, 97, 98, 2001, Noah S. Friedman
;; Copyright (C) 2010-2012, Ian Eure

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: Ian Eure <ian.eure@gmail.com>
;; Maintainer: friedman@splode.com
;; Version: 1.2
;; Package-Version: 20120904.2042
;; Package-Commit: 812e27409d01c38d74906a1816640506d6e7e3ef
;; Keywords: unix, comm
;; Created: 1996-07-03

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Support for remote logins using `ssh'.
;; This program is layered on top of shell.el; the code here only accounts
;; for the variations needed to handle a remote process, e.g. directory
;; tracking and the sending of some special characters.

;; If you wish for ssh mode to prompt you in the minibuffer for
;; passwords when a password prompt appears, just enter m-x send-invisible
;; and type in your line, or add `comint-watch-for-password-prompt' to
;; `comint-output-filter-functions'.

;;; Code:

(require 'comint)
(require 'shell)

(defgroup hc-ssh nil
  "Secure remote login interface"
  :group 'processes
  :group 'unix)

(defcustom hc-ssh-program "ssh"
  "*Name of program to invoke ssh"
  :type 'string
  :group 'hc-ssh)

(defcustom hc-ssh-explicit-args '()
  "*List of arguments to pass to ssh on the command line."
  :type '(repeat (string :tag "Argument"))
  :group 'hc-ssh)

(defcustom hc-ssh-mode-hook nil
  "*Hooks to run after setting current buffer to hc-ssh-mode."
  :type 'hook
  :group 'hc-ssh)

(defcustom hc-ssh-process-connection-type t
  "*If non-`nil', use a pty for the local ssh process.
If `nil', use a pipe (if pipes are supported on the local system).

Generally it is better not to waste ptys on systems which have a static
number of them.  However, ssh won't allocate a pty on the remote host
unless one is used locally as well."
  :type '(choice (const :tag "ptys" t)
		 (const :tag "pipes" nil))
  :group 'hc-ssh)

(defcustom hc-ssh-directory-tracking-mode 'local
  "*Control whether and how to do directory tracking in an ssh buffer.

nil means don't do directory tracking.

t means do so using an ftp remote file name.

Any other value means do directory tracking using local file names.
This works only if the remote machine and the local one
share the same directories (through NFS).  This is the default.

This variable becomes local to a buffer when set in any fashion for it.

It is better to use the function of the same name to change the behavior of
directory tracking in an ssh session once it has begun, rather than
simply setting this variable, since the function does the necessary
re-synching of directories."
  :type '(choice (const :tag "off" nil)
		 (const :tag "ftp" t)
		 (const :tag "local" local))
  :group 'hc-ssh)

(make-variable-buffer-local 'hc-ssh-directory-tracking-mode)

(defcustom hc-ssh-x-display-follow-current-frame t
  "*Control X display used by ssh for X tunneling.
If non-nil and ssh is configured to enable remote X display forwarding,
the display of the current emacs frame will be used rather than the display
to which the emacs process was originally launched.  \(These may be
different if currently using a remote frame.\)"
  :type 'boolean
  :group 'hc-ssh)

(defcustom hc-ssh-host nil
  "*The name of the remote host.  This variable is buffer-local."
  :type '(choice (const nil) string)
  :group 'hc-ssh)

(defcustom hc-ssh-remote-user nil
  "*The username used on the remote host.
This variable is buffer-local and defaults to your local user name.
If ssh is invoked with the `-l' option to specify the remote username,
this variable is set from that."
  :type '(choice (const nil) string)
  :group 'hc-ssh)

;; Initialize hc-ssh mode map.
(defvar hc-ssh-mode-map '())
(cond
 ((null hc-ssh-mode-map)
  (setq hc-ssh-mode-map (if (consp shell-mode-map)
                            (cons 'keymap shell-mode-map)
                          (copy-keymap shell-mode-map)))
  (define-key hc-ssh-mode-map "\C-c\C-c" 'hc-ssh-send-Ctrl-C)
  (define-key hc-ssh-mode-map "\C-c\C-d" 'hc-ssh-send-Ctrl-D)
  (define-key hc-ssh-mode-map "\C-c\C-z" 'hc-ssh-send-Ctrl-Z)
  (define-key hc-ssh-mode-map "\C-c\C-\\" 'hc-ssh-send-Ctrl-backslash)
  (define-key hc-ssh-mode-map "\C-d" 'hc-ssh-delchar-or-send-Ctrl-D)
  (define-key hc-ssh-mode-map "\C-i" 'hc-ssh-tab-or-complete)))


;;;###autoload (add-hook 'same-window-regexps "^\\*hc-ssh-.*\\*\\(\\|<[0-9]+>\\)")

(defvar hc-ssh-history nil)

;;;###
(defun hc-ssh-hostname-at-point ()
  (let ((hostname (thing-at-point 'url)))
    (and hostname (substring-no-properties hostname 7))))

;;;###autoload
(defun hc-ssh (buffer host-maybe-user &rest args)
  (interactive)

  (let* ((process-connection-type hc-ssh-process-connection-type)
         (host-parts (split-string host-maybe-user "@"))
         (host (car (last host-parts)))
         (user (or (cadr (member "-l" args))
                   (if (= 2 (length host-parts)) (car host-parts))
                   (user-login-name)))
         buffer-name
         proc)

    (setq args (cons host-maybe-user args))

    (and hc-ssh-explicit-args
         (setq args (append hc-ssh-explicit-args args)))

    (cond ((null buffer)
           (error "must supply a buffer or buffer name"))
	  ((stringp buffer)
	   (setq buffer-name buffer))
          ((bufferp buffer)
           (setq buffer-name (buffer-name buffer)))
          (t
           (error "2nd args must be a buffer or string")))

    (setq buffer (get-buffer-create buffer-name))
    (pop-to-buffer buffer-name)

    (cond
     ((comint-check-proc buffer-name))
     (t
      (hc-ssh-with-check-display-override
       #'(lambda ()
           (comint-exec buffer buffer-name hc-ssh-program nil args)))
      (setq proc (get-buffer-process buffer))
      ;; Set process-mark to point-max in case there is text in the
      ;; buffer from a previous exited process.
      (set-marker (process-mark proc) (point-max))

      ;; comint-output-filter-functions is treated like a hook: it is
      ;; processed via run-hooks or run-hooks-with-args in later versions
      ;; of emacs.
      ;; comint-output-filter-functions should already have a
      ;; permanent-local property, at least in emacs 19.27 or later.
      (cond
       ((fboundp 'make-local-hook)
        (make-local-hook 'comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'hc-ssh-carriage-filter nil t))
       (t
        (make-local-variable 'comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'hc-ssh-carriage-filter)))

      (hc-ssh-mode)

      (make-local-variable 'hc-ssh-host)
      (setq hc-ssh-host host)
      (make-local-variable 'hc-ssh-remote-user)
      (setq hc-ssh-remote-user user)

      (condition-case ()
          (cond ((eq hc-ssh-directory-tracking-mode t)
                 ;; Do this here, rather than calling the tracking mode
                 ;; function, to avoid a gratuitous resync check; the default
                 ;; should be the user's home directory, be it local or remote.
                 (setq comint-file-name-prefix
                       (concat "/" hc-ssh-remote-user "@" hc-ssh-host ":"))
                 (cd-absolute comint-file-name-prefix))
                ((null hc-ssh-directory-tracking-mode))
                (t
                 (cd-absolute (concat comint-file-name-prefix "~/"))))
        (error nil)))))
  buffer)

(put 'hc-ssh-mode 'mode-class 'special)

(defun hc-ssh-mode ()
  "Set major-mode for ssh sessions.
If `ssh-mode-hook' is set, run it."
  (interactive)
  (kill-all-local-variables)
  (shell-mode)
  (setq major-mode 'hc-ssh-mode)
  (setq mode-name "hc-ssh")
  (use-local-map hc-ssh-mode-map)
  (setq shell-dirtrackp hc-ssh-directory-tracking-mode)
  (make-local-variable 'comint-file-name-prefix)
  (run-hooks 'hc-ssh-mode-hook))

(defun hc-ssh-directory-tracking-mode (&optional prefix)
  "Do remote or local directory tracking, or disable entirely.

If called with no prefix argument or a unspecified prefix argument (just
``\\[universal-argument]'' with no number) do remote directory tracking via
ange-ftp.  If called as a function, give it no argument.

If called with a negative prefix argument, disable directory tracking
entirely.

If called with a positive, numeric prefix argument, e.g.
``\\[universal-argument] 1 M-x hc-ssh-directory-tracking-mode\'',
then do directory tracking but assume the remote filesystem is the same as
the local system.  This only works in general if the remote machine and the
local one share the same directories (through NFS)."
  (interactive "P")
  (cond
   ((or (null prefix)
        (consp prefix))
    (setq hc-ssh-directory-tracking-mode t)
    (setq shell-dirtrackp t)
    (setq comint-file-name-prefix
          (concat "/" hc-ssh-remote-user "@" hc-ssh-host ":")))
   ((< prefix 0)
    (setq hc-ssh-directory-tracking-mode nil)
    (setq shell-dirtrackp nil))
   (t
    (setq hc-ssh-directory-tracking-mode 'local)
    (setq comint-file-name-prefix "")
    (setq shell-dirtrackp t)))
  (cond
   (shell-dirtrackp
    (let* ((proc (get-buffer-process (current-buffer)))
           (proc-mark (process-mark proc))
           (current-input (buffer-substring proc-mark (point-max)))
           (orig-point (point))
           (offset (and (>= orig-point proc-mark)
                        (- (point-max) orig-point))))
      (unwind-protect
          (progn
            (delete-region proc-mark (point-max))
            (goto-char (point-max))
            (shell-resync-dirs))
        (goto-char proc-mark)
        (insert current-input)
        (if offset
            (goto-char (- (point-max) offset))
          (goto-char orig-point)))))))

;; Check to see if we should override the X display name that the ssh
;; process will inherit from the environment, which could affect where
;; remote clients will appear when using X forwarding.
;;
;; If ssh-x-display-follow-current-frame is non-nil, this function
;; overrides the process-environment display for the called function.
(defun hc-ssh-with-check-display-override (fn)
  (let (frame-disp emacs-disp)
    (cond ((and hc-ssh-x-display-follow-current-frame
                (eq window-system 'x)
                (setq frame-disp (cdr (assq 'display (frame-parameters))))
                (setq emacs-disp (getenv "DISPLAY"))
                ;; setenv is expensive, so don't do all that work if
                ;; there's no point.
                (not (string= frame-disp emacs-disp)))
           ;; Don't shadow process-environment completely because the
           ;; called function might legitimately want to modify other
           ;; environment variables permanently; just save and restore
           ;; original global display value.
           (unwind-protect
               (progn
                 (setenv "DISPLAY" frame-disp)
                 (funcall fn))
             (setenv "DISPLAY" emacs-disp)))
          (t
           (funcall fn)))))

(defun hc-ssh-carriage-filter (string)
  (let* ((point-marker (point-marker))
         (end (process-mark (get-buffer-process (current-buffer))))
         (beg (or (and (boundp 'comint-last-output-start)
                       comint-last-output-start)
                  (- end (length string)))))
    (goto-char beg)
    (while (search-forward "\C-m" end t)
      (delete-char -1))
    (goto-char point-marker)))

(defun hc-ssh-send-Ctrl-C ()
  (interactive)
  (process-send-string nil "\C-c"))

(defun hc-ssh-send-Ctrl-D ()
  (interactive)
  (process-send-string nil "\C-d"))

(defun hc-ssh-send-Ctrl-Z ()
  (interactive)
  (process-send-string nil "\C-z"))

(defun hc-ssh-send-Ctrl-backslash ()
  (interactive)
  (process-send-string nil "\C-\\"))

(defun hc-ssh-delchar-or-send-Ctrl-D (arg)
  "\
Delete ARG characters forward, or send a C-d to process if at end of buffer."
  (interactive "p")
  (if (eobp)
      (hc-ssh-send-Ctrl-D)
    (delete-char arg)))

(defun hc-ssh-tab-or-complete ()
  "Complete file name if doing directory tracking, or just insert TAB."
  (interactive)
  (if hc-ssh-directory-tracking-mode
      (comint-dynamic-complete)
    (insert "\C-i")))

(provide 'hc-ssh)

;;; ssh.el ends here
