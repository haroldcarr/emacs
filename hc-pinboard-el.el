;;; hc-pinboard-el.el --- pinboard-el          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(use-package pinboard)

(define-derived-mode pinboard-mode tabulated-list-mode "Pinboard Mode"
  "Major mode for handling a list of Pinboard pins.

The key bindings for `pinboard-mode' are:

\\{pinboard-mode-map}"
  (setq tabulated-list-format
        [("Description" 60 t)
         ("Time"         8 t)
         ("Tags"        30 t)
         ("URL"         30 t)])
  (tabulated-list-init-header)
  (setq tabulated-list-sort-key '("Time" . t)))

(setq pinboard-time-format-function
      (lambda
        (time)
        (format-time-string "%y-%m-%d"
                            (parse-iso8601-time-string time))))

(defun pinboard-redraw (&optional filter)
  "Redraw the pin list.

Optionally filter the list of pins to draw using the function
FILTER."
  ;; If there is no filter...
  (unless filter
    ;; ...ensure any ongoing tagging filter gets cleared.
    (setq pinboard-tag-filter nil))
  (cl-flet ((highlight (s pin)
                       (propertize s 'font-lock-face
                                   (if (string= (alist-get 'toread pin) "yes")
                                       'pinboard-unread-face
                                     'pinboard-read-face))))
    (setq tabulated-list-entries
          (mapcar (lambda (pin)
                    (list
                     (alist-get 'hash pin)
                     (vector
                      (highlight (alist-get 'description pin) pin)
                      (highlight (funcall pinboard-time-format-function (alist-get 'time pin)) pin)
                      (highlight (alist-get 'tags pin) pin)
                      (highlight (alist-get 'href pin) pin))))
                  (seq-filter
                   (setq pinboard-last-filter (or filter #'identity))
                   (pinboard-get-pins)))))
  (tabulated-list-print t))

(defun hc-pinboard-refresh-time ()
  (interactive)
  (setq tabulated-list-sort-key '("Time" . t))
  (pinboard-refresh))

(defun hc-pinboard-refresh-tags ()
  (interactive)
  (setq tabulated-list-sort-key '("Tags" . t))
  (pinboard-refresh))

(provide 'hc-pinboard-el)

;;; hc-pinboard-el.el ends here
