;;; hc-gnus.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(hcSection "GNUS")

;; https://whereofwecannotspeak.wordpress.com/2009/07/15/getting-gnus-to-read-mail-over-imap/

;; Get an app-specific password: https://support.google.com/accounts/answer/185833

;;;; RECEIVE
;; https://lars.ingebrigtsen.no/2020/01/06/whatever-happened-to-news-gmane-org/comment-page-1/#comment-36418
(defvar gnus-select-method)
(setq gnus-select-method
      '(nntp "news.gmane.io")
      )

;; (defvar gnus-secondary-select-methods)
;; (setq gnus-secondary-select-methods
;;       '((nnimap "gmail"
;;                 (nnimap-address "imap.gmail.com")
;;                 (nnimap-server-port 993)
;;                 (nnimap-authenticator login)
;;                 (nnimap-expunge-on-close 'never)
;;                 (nnimap-stream ssl))))

;; Original value was  "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
(defvar gnus-summary-line-format)
(defvar gnus-user-date-format-alist)
(setq gnus-summary-line-format "%&user-date;%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d")))

;;;; SEND
(defvar message-send-mail-function)
(defvar smtpmail-starttls-credentials)
(defvar smtpmail-auth-credentials)
(defvar smtpmail-default-smtp-server)
(defvar smtpmail-smtp-server)
(defvar smtpmail-smtp-service)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "harold.carr@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mail-host-address "harold.carr@gmail.com") ;; gets rid of "tickle me"

;; M-x gnus

;; when it prompts for your password, give app-specific password
;; -  (and optionally let it store that password ---unprotected--- in =~/.authinfo=)

(provide 'hc-gnus)

;;; hc-gnus.el ends here
