;;; hc-daemon-emacsclient.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; See (I could not get C-c e to work in IntelliJ)
;; http://spin.atomicobject.com/2014/08/07/intellij-emacs/

;; # manual start
;; ./bin-hosted/emacs --daemon
;; # manual use
;; ./bin-hosted/emacsclient -c <any file/dir name>
;; # manual kill
;; # - from within emacs
;; M-x kill-emacs
;; or
;; M-x save-buffers-kill-emacs
;; # from outside of emacs
;; emacsclient -e '(kill-emacs)'
;; or
;; emacsclient -e '(client-save-kill-emacs)'

(require 'server)

(defun server-started-p ()
  "Return non-nil if this Emacs has a server started."
  (and (boundp 'server-process) server-process))

(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24)
               (not (server-started-p)))
      (server-start)))

(provide 'hc-daemon-emacsclient)

;;; hc-daemon-emacsclient.el ends here
