;; read this (and the blog it points to)
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

;; ---------------------------------------------------------------------------

;; https://github.com/tsengf/theme-cycle/blob/main/theme-cycle.el

;; Idea borrowed from karls
;; https://gist.github.com/karls/9d0d5e25cd72d633893f

;; Theme changer
(defvar hc-curr-theme-index 0)
(defvar hc-theme-list
  (custom-available-themes) ;; Get a list of all supported themes
  ;; Or replace with a list of themes to cycle through
  ;;'(ef-autumn ef-bio ef-cherie ef-dark ef-deuteranopia-dark ef-duo-dark ef-elea-dark ef-maris-dark ef-night ef-symbiosis ef-trio-dark ef-tritanopia-dark ef-winter)
  "List of themes to cycle through.")

(defun hc-get-theme-name (idx)
  (nth idx hc-theme-list))

(defun hc-cycle-themes (&optional up-down)
  (interactive)
  (let* (theme
         next-theme
         next-theme-index
         (num-themes (length hc-theme-list)))
    (setq theme (hc-get-theme-name hc-curr-theme-index))
    (if up-down
        (setq next-theme-index (mod (+ 1 hc-curr-theme-index) num-themes))
      (setq next-theme-index (mod (+ (- num-themes 1) hc-curr-theme-index) num-themes)))
    (setq next-theme (hc-get-theme-name next-theme-index))
    (setq hc-curr-theme-index next-theme-index)
    (disable-theme theme)
    (load-theme next-theme t)
    (message "Load theme %d/%d %s" next-theme-index num-themes (hc-get-theme-name hc-curr-theme-index))))

;; Use F10 and shift-F10 to cycle through themes
(when window-system
  (global-set-key (kbd "<f10>") (lambda () (interactive) (hc-cycle-themes 1))))
  (global-set-key (kbd "S-<f10>") 'hc-cycle-themes)

;; ---------------------------------------------------------------------------

(provide 'hc-theme-switch)
