;;; hc-sync --- sync  -*- lexical-binding: t -*-

(require 'dash)
(require 'hc-run-command)
(require 'hc-ssh)
(require 's)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar *hc-sync-to*)
(defvar *hc-sync-what*)

(defun hc-add-or-delete (widget this lis)
  (if (widget-get widget :value)
      (cons this lis)
    (-remove #'(lambda (that) (eq this that)) lis)))

(defun hc-add-or-delete-*hc-sync-to* (widget this)
  (setq *hc-sync-to* (hc-add-or-delete widget this *hc-sync-to*))
  (message (prin1-to-string *hc-sync-to*)))

(defun hc-add-or-delete-*hc-sync-what* (widget this)
  (setq *hc-sync-what* (hc-add-or-delete widget this *hc-sync-what*))
  (message (prin1-to-string *hc-sync-what*)))

(defun hc-host-to-ip       (h) (shell-command-to-string (concat "ip-"       h)))
(defun hc-host-to-username (h) (shell-command-to-string (concat "username-" h)))

(defun hc-sync ()
  "Synchronize data between machines."
  (interactive)
  (switch-to-buffer "*HC Sync*")
  (kill-all-local-variables)
  (make-local-variable '*hc-sync-to*)
  (make-local-variable '*hc-sync-what*)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let ((here (s-trim (shell-command-to-string "hostname")))
        (to-o2011)
        (to-o2015)
        (to-o2020))
    (setq *hc-sync-to*   '())
    (setq *hc-sync-what* '())

    (widget-insert "\n--------------------------------------------------")
    (widget-insert "\nfrom:\n\n")
    (widget-create 'radio-button-choice
                   :value  here
                   `(item ,here))

    (widget-insert "\n--------------------------------------------------")
    (widget-insert "\nto (select one or more):\n\n")

    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-to* w "o2011"))
                   nil)
    (widget-insert " o2011 ")
    (setq to-o2011
          (widget-create 'editable-field
                         :format "%v"
                         "?"))

    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-to* w "o2015"))
                   nil)
    (widget-insert " o2015 ")
    (setq to-o2015
          (widget-create 'editable-field
                         :format "%v"
                         "?"))

    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-to* w "o2020"))
                   nil)
    (widget-insert " o2020 ")
    (setq to-o2020
          (widget-create 'editable-field
                         :format "%v"
                         "?"))

    (widget-insert "\n--------------------------------------------------")
    (widget-insert "\nwhat (select one or more):\n\n")

    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-what* w "s-a2b"))
                   nil)
    (widget-insert " s-a2b")

    (widget-insert "\n")
    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-what* w "s-fcw-music"))
                   nil)
    (widget-insert " s-fcw-music")

    (widget-insert "\n")
    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-what* w "s-hc-music"))
                   nil)
    (widget-insert " s-hc-music")

    (widget-insert "\n")
    (widget-create 'checkbox
                   :notify (lambda (w &rest ignore) (hc-add-or-delete-*hc-sync-what* w "s-pictures"))
                   nil)
    (widget-insert " s-pictures")

    (widget-insert "\n")
    (widget-insert "\n--------------------------------------------------")
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify
                     (lambda (&rest _ignore)
                       (-each *hc-sync-to*
                         (lambda (to)
                           (let ((user (hc-host-to-username to))
                                 (ip   (hc-host-to-ip       to)))
                             (message "%s %s %s" user to ip)
                             (sleep-for 1)
                             (hc-ssh-get-hostname
                              user ip
                              #'(lambda (r)
                                  (pcase r
                                    ("o2011" (widget-value-set
                                              to-o2011 (format "%s %s %s" user r ip)))
                                    ("o2015" (widget-value-set
                                              to-o2015 (format "%s %s %s" user r ip)))
                                    ("o2020" (widget-value-set
                                              to-o2020 (format "%s %s %s" user r ip)))
                                    (err     (message "unknown %S" err)))))))))
                   "check connectivity")

    ;; --------------------------------------------------
    (widget-insert "\n")
    (widget-create
     'push-button
     :notify
       (lambda (&rest ignore)
         (-each *hc-sync-to*
           #'(lambda (to)
               (-each *hc-sync-what*
                 #'(lambda (what)
                     (let ((action (format "%s-%s-to-%s" what here to)))
                       (message action)
                       (hc-run-sync action)))))))
     "sync")

    ;; --------------------------------------------------
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore) (hc-sync))
                   "reset")

    ;; --------------------------------------------------
    (widget-insert "\n")

    ;; --------------------------------------------------
    (use-local-map widget-keymap)
    (widget-setup)))


(defun hc-run-sync (sync-name)
  (hcRunCommandInBuffer (concat "*" sync-name "*") sync-name))

(defun hc-ssh-get-hostname (user host handle-result)
  (interactive)
  (let* ((user-at-host (concat user "@" host))
         (buffer (hc-ssh (concat "*SSH-" user-at-host "*")
                         host "-l" user
                         "-o" "RemoteCommand hostname"
                         "-o" "ConnectTimeout 1"))
         (proc (get-buffer-process buffer)))
    (set-process-sentinel
     proc
     #'(lambda (_process event)
         (let ((name (cond ((string-equal event "finished\n")
                            (with-current-buffer buffer
                              (goto-char (point-min))
                              (forward-line 1)
                              (current-word)))
                           (t event))))
           (kill-buffer buffer)
           (funcall handle-result name))))))

(provide 'hc-sync)

;;; hc-sync.el ends here
