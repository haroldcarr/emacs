;; Read this (and the blog in points to.
;; https://www.unwoundstack.com/blog/switching-emacs-themes.html

(defun hc-disable-all-themes ()
  "Disable all currently enabled themes, as defined by `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun hc-load-theme (theme &optional no-confirm no-enable)
  "Disable current themes, then load a new one."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))
    nil nil))
  (message "before")
  (hc-disable-all-themes)
  (message "after")
  (load-theme theme no-confirm no-enable))
