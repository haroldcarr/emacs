;;; emc-widget-dynamic-group.el --- widget                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://within-parens.blogspot.com/2025/04/emacs-lisp-programming-with-deepseek.html

(require 'widget)
(require 'wid-edit)


(define-widget 'dynamic-group 'default
  "A container widget that dynamically manages child widgets in a column."
  :format "%v"
  :value ()
  :tag "Dynamic Group"
  :args nil

  ;; Core widget methods
  :create (lambda (widget)
            (let ((inhibit-read-only t))
              (widget-put widget :from (point))
              (dolist (child (reverse (widget-get widget :children)))
                (widget-create child))
              (widget-put widget :to (point))))

  :value-get (lambda (widget)
               (mapcar (lambda (child)
                         (widget-apply child :value-get))
                       (widget-get widget :children)))

  :value-set (lambda (widget value)
               (widget-put widget :value value))

  :value-delete (lambda (widget)
                  (dolist (child (widget-get widget :children))
                    (widget-apply child :value-delete)))

  :validate (lambda (widget)
              (let ((children (widget-get widget :children)))
                (catch :invalid
                  (dolist (child children)
                    (when (widget-apply child :validate)
                      (throw :invalid child)))
                  nil)))
  )


(defun dynamic-group-add (widget type &rest args)
  "Add a new widget (of TYPE and ARGS to the WIDGET group."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (widget-get widget :to))
      (let ((child (apply 'widget-create (append (list type) args))))
        (widget-put widget
		    :children (cons child (widget-get widget :children)))
        (widget-put widget
		    :to (point))
        (widget-value-set widget
          (cons (widget-value child) (widget-value widget)))))
    (widget-setup)))


(defun dynamic-group-remove (widget)
  "Remove the last widget from the WIDGET group."
  (when-let ((children (widget-get widget :children)))
    (let ((inhibit-read-only t)
          ;; (child (car children))
	  )
      (save-excursion
        (goto-char (widget-get widget :from))
        (delete-region (point) (widget-get widget :to))
        (widget-put widget :children (cdr children))
        (dolist (c (reverse (widget-get widget :children)))
          (widget-create c))
        (widget-put widget :to (point))
        (widget-value-set widget
			  (mapcar 'widget-value
				  (widget-get widget :children)))
        (widget-setup)))))


(defun demo-dynamic-group ()
  "Test the dynamic-group widget."
  (interactive)
  (switch-to-buffer "*Dynamic Group Demo*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer)

    (widget-insert "* Dynamic Group Demo\n\n")

    ;; Now I create the `dynamic-group'.

    (let ((group (widget-create 'dynamic-group)))
      (widget-insert "\n")

      ;; The rest are just two buttons testing the widget's behavior,
      ;; invoking`dynamic-group-add' and `dynamic-group-remove'.

      (widget-create
       'push-button
       :notify
       (lambda (&rest _)
         (dynamic-group-add group 'string
			    :format "Text: %v\n"
			    :value (format "Item %d"
					   (1+ (length (widget-get group :children))))))
       "(+) Add Field (Click Anywhere)")

      (widget-insert " ")

      (widget-create
       'push-button
       :notify (lambda (&rest _)
		 (dynamic-group-remove group))
       "(-) Remove Last")

      (widget-insert "\n"))

    ;; Wrap everything up using the `widget-keymap' and `widget-setup'
    ;; functions.

    (use-local-map widget-keymap)
    (widget-setup)))


(provide 'emc-widget-dynamic-group)

;;; emc-widget-dynamic-group.el ends here
