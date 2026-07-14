;;; hc-appearance.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(hcSection "appearance")

;; NOTE: see end of file for a call that is made during load.

;; ------------------------------------------------------------------------------

(defun hc-appearance-startup ()
  (interactive)
  (set-frame-height (selected-frame) 88) ;; 100
  (set-frame-width  (selected-frame) 23) ;;  27                 20
  (set-face-font 'default "-apple-Monaco-medium-normal-normal-*-24-*-*-*-m-0-iso10646-1")
  (let ((monitor-name (hc-get-available-monitor-name)))
    (cond (monitor-name
           (hc-move-frame-to-monitor monitor-name)
           (pcase monitor-name
             ("portrait"  (progn (hc-font-size 16) (hc-resize-frame "full")))
             ("landscape" (progn                   (hc-resize-frame "left-2/3")))
             ("laptop"    (progn (hc-font-size 18) (hc-resize-frame "full"))))))))

;; ------------------------------------------------------------------------------

(defconst hc-monitor-mm-sizes
  '(("laptop"    . (301 196))
    ("portrait"  . (349 609))   ;; utah
    ("landscape" . (602 338)))) ;; utah

(defun hc-get-available-monitor-name ()
  "Return the first available monitor name in priority order."
  (let ((available-mm-sizes (mapcar (lambda (attrs) (alist-get 'mm-size attrs))
                                    (display-monitor-attributes-list))))
    (seq-find (lambda (name) (member (cdr (assoc name hc-monitor-mm-sizes))
                                     available-mm-sizes))
              '("portrait" "landscape" "laptop"))))

(defun hc-move-frame-to-monitor (&optional name)
  "Move selected frame to monitor NAME."
  (interactive)
  (let* ((name    (or name (hc-read-monitor-name)))
         (name    (if (symbolp name) (symbol-name name) name))
         (mm-size (cdr (assoc name hc-monitor-mm-sizes)))
         (monitor (seq-find (lambda (attrs) (equal (alist-get 'mm-size attrs) mm-size))
                            (display-monitor-attributes-list))))
    (unless mm-size (user-error "Unknown monitor: %s"  name))
    (unless monitor (user-error "Monitor %s not found" name))
    (pcase-let ((`(,x ,y ,_w ,_h) (alist-get 'workarea monitor)))
      (set-frame-position (selected-frame) x y))))

(defun hc-read-monitor-name ()
  (completing-read "Monitor: " hc-monitor-mm-sizes nil t))

;; ------------------------------------------------------------------------------

(defconst hc-frame-size-options '("full" "left-2/3"))

(defun hc-resize-frame (&optional size)
  "Resize selected frame to SIZE on its current monitor,
where SIZE is either \"full\" or \"left-2/3\"."
  (interactive)
  (let* ((size      (or size (hc-read-frame-size-option)))
         (workarea  (alist-get 'workarea (frame-monitor-attributes)))
         (x         (nth 0 workarea))
         (y         (nth 1 workarea))
         (width     (nth 2 workarea))
         (height    (nth 3 workarea))
         (new-width (pcase size
                      ("full" width)
                      ("left-2/3" (/ (* width 2) 3))
                      (_ (user-error "Unknown frame size: %s" size)))))
    (set-frame-position (selected-frame) x y)
    (set-frame-size (selected-frame) new-width height t)))

(defun hc-read-frame-size-option ()
  (completing-read "Frame size: " hc-frame-size-options nil t))

;; ------------------------------------------------------------------------------
;; frame/font size

;; these run text-scale-adjust from face-remap.el
;; C-x C-- : decrease font size
;; C-x C-+ : increase font size
;; C-x C-0 : reset to default size

(defun hc-font-size (n) "N."
  (interactive "nSize: ")
  (hcFonts (* n 10) (* n 15)))

(defun hcFonts (default-height variable-pitch-height)
  "DEFAULT-HEIGHT VARIABLE-PITCH-HEIGHT."
  (interactive)
  (set-face-attribute 'default        nil :family "Monaco" :height default-height)
  (set-face-attribute 'variable-pitch nil :family "Monaco" :height variable-pitch-height
                                                           :weight 'regular))

;; https://github.com/zonuexe/emacs-presentation-mode
;; this one changes font size but keeps frame size the same
(use-package presentation)

;; ------------------------------------------------------------------------------

(cond ((hcDarwinP) (hc-appearance-startup)))

(provide 'hc-appearance)

;;; hc-appearance.el ends here
