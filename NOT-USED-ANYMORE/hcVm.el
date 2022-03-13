(defun hcVm ()
  ;; requires 20.4 or later
  (hcPushOn (concat *usrLocalHcDir*  "/emacs/vm/vm-6.71") load-path)
  ;; load explicit for now
  (load-library "vm-version.el")	; first
  (load-library "tapestry.el")
  (load-library "vm-byteopts.el")
  (load-library "vm-delete.el")
  (load-library "vm-digest.el")
  (load-library "vm-easymenu.el")
  (load-library "vm-edit.el")
  (load-library "vm-folder.el")
  (load-library "vm-imap.el")
  (load-library "vm-license.el")
  (load-library "vm-macro.el")
  (load-library "vm-mark.el")
  (load-library "vm-menu.el")
  (load-library "vm-message.el")
  (load-library "vm-mime.el")
  (load-library "vm-minibuf.el")
  (load-library "vm-misc.el")
  (load-library "vm-motion.el")
  (load-library "vm-mouse.el")
  (load-library "vm-page.el")
  (load-library "vm-pop.el")
  (load-library "vm-reply.el")
  (load-library "vm-save.el")
  (load-library "vm-search.el")
  (load-library "vm-sort.el")
  (load-library "vm-startup.el")
  (load-library "vm-summary.el")
  (load-library "vm-thread.el")
  (load-library "vm-toolbar.el")
  (load-library "vm-undo.el")
  (load-library "vm-user.el")
  (load-library "vm-vars.el")
  (load-library "vm-virtual.el")
  (load-library "vm-window.el")
  ;; From README
  ;;(autoload 'vm             "vm" "Start VM on your primary inbox." t)
  ;;(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
  ;;(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
  ;;(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
  ;;(autoload 'vm-mode         "vm" "Run VM major mode on a buffer" t)
  ;;(autoload 'vm-mail         "vm" "Send a mail message using VM." t)
  ;;(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)
  ;; From VM User's Manual - Starting Up
  ;; * set ~/.vm location
  (setq vm-auto-get-new-mail nil)	; do not get new mail yet
   ;;;(setq vm-primary-inbox (concat *hc-gnu-dir* "/.w3/INBOX"))
  ;; * set crash box location
  (setq vm-imap-max-message-size 10000)
  (setq vm-imap-messages-per-session 1)
  (setq vm-imap-bytes-per-session 10000)
  (setq vm-imap-expunge-after-retrieving nil)
  ;;``imap:HOST:PORT:MAILBOX:AUTH:USER:PASSWORD''
  (setq vm-spool-files
	'(("~/INBOX" "imap:shorter.eng.sun.com:143:inbox:login:carr:*" "~/INBOX.CRASH")))
  )

;;; End of file.
