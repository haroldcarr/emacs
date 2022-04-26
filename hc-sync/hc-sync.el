;;; hc-sync --- sync  -*- lexical-binding: t -*-

(require 'dash)
(require 'hc-run-command)
(require 'hc-ssh)
(require 's)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar *hc-sync-to*)

(defun hc-sync ()
  "Synchronize data between machines."
  (interactive)
  (switch-to-buffer "*HC Sync*")
  (kill-all-local-variables)
  (make-local-variable '*hc-sync-to*)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let* ((here       (s-trim (shell-command-to-string "hostname")))
         (candidates (reverse
                      (-filter (lambda (x) (s-contains? (concat here "-to") x))
                               (directory-files (concat (hcLocation "dotfiles") "/.unison/"))))))
    (setq *hc-sync-to* '())

    ;; --------------------------------------------------
    (-each (hc-group candidates)
      #'(lambda (g)
          (widget-insert "\n")
          (-each g
            #'(lambda (c)
                (widget-insert "\n")
                (hc-sync-to-what c)))))

    ;; --------------------------------------------------
    (widget-insert "\n\n")
    (widget-create
     'push-button
     :notify
       #'(lambda (&rest ignore)
           (message *hc-sync-what*)
           (hc-run-sync *hc-sync-what*))
     "sync")

    ;; --------------------------------------------------
    (widget-insert "\n")

    ;; --------------------------------------------------
    (use-local-map widget-keymap)
    (widget-setup)))

(defmacro hc-sync-to-what (what)
  `(progn
     (widget-create 'checkbox
                    :notify #'(lambda (w &rest ignore)
                                (setq *hc-sync-what* ,what)
                                (message (prin1-to-string *hc-sync-what*)))
                    nil)
     (widget-insert (concat " " ,what))))

(defun hc-run-sync (sync-name)
  (hcRunCommandInBuffer (concat "*" sync-name "*") sync-name))

(defun hc-group (fs)
  (if (null fs) '()
    (let* ((g (-group-by #'(lambda (f) (s-starts-with? (s-left 8 (car fs)) f)) fs))
           (this (pcase g ((and (pred listp)
                                l
                                (guard (eq (caar l) t)))
                           (cdar l))))
           (that (pcase g ((and (pred listp)
                                l
                                (guard (eq (caadr l) nil)))
                           (cdadr l)))))
      (cons this (hc-group that)))))

(provide 'hc-sync)

;;; hc-sync.el ends here






