;;; hc-appearance.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------------------------------------------------------
;; * frame/font size

(hcSection "frame/font size")

;; C-U C-X : shows current font

;; C-x C-- : decrease font size
;; C-x C-+ : increase font size
;; C-x C-0 : reset to default size
;; these run text-scale-adjust

(defun hc-h (n)    "N."   (interactive) (set-frame-height (selected-frame) n))
(defun hc-w (n)    "N."   (interactive) (set-frame-width (selected-frame) n))
(defun hc-hw (x y) "X Y." (interactive) (hc-h x) (hc-w y))
(defun hc-hwd ()   "."    (interactive) (hc-h 27) (hc-w 101))

(defun hcFonts (default-height variable-pitch-height)
  "DEFAULT-HEIGHT VARIABLE-PITCH-HEIGHT."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Monaco"
                      :height variable-pitch-height
                      :weight 'regular))

(defun hcDoFonts () "."
  (interactive)
  (when window-system
    (cond ((> (x-display-pixel-width) 1800)
           (hcFonts 200 300))
          (t (hcFonts 175 200)))))

(defun hc-font-size (n) "N."
  (interactive "nSize: ")
  (hcFonts (* n 10) (* n 15)))

;; ------------------------------------------------------------------------------
;; https://github.com/zonuexe/emacs-presentation-mode

;; this one changes font size but keeps frame size the same

(use-package presentation)

;; ------------------------------------------------------------------------------
;; http://emacsninja.com/posts/making-emacs-more-presentable.html

;; this one shrinks/enlarges frame

(defun hc-alter-frame-font-size (fn)
  (let* ((current-font-name (frame-parameter nil 'font))
         (decomposed-font-name (x-decompose-font-name current-font-name))
         (font-size (string-to-number (aref decomposed-font-name 5))))
    (aset decomposed-font-name 5 (int-to-string (funcall fn font-size)))
    (set-frame-font (x-compose-font-name decomposed-font-name))))

(defun hc-inc-frame-font-size ()
  (interactive)
  (hc-alter-frame-font-size '1+))

(defun hc-dec-frame-font-size ()
  (interactive)
  (hc-alter-frame-font-size '1-))

;; (global-set-key (kbd "C-c C-+") 'hc-inc-frame-font-size)
;; (global-set-key (kbd "C-c C-=") 'hc-inc-frame-font-size)
;; (global-set-key (kbd "C-c C--") 'hc-dec-frame-font-size)

;; ------------------------------------------------------------------------------

(defun hcMacFW () "."
  (interactive)
  (hc-w 88) ;; 100
  (hc-h 23) ;;  27                                              20
  (set-face-font 'default "-apple-Monaco-medium-normal-normal-*-24-*-*-*-m-0-iso10646-1")
  )

(defun hcColorZB () "."
   (interactive)
   (set-foreground-color "#DCDCCC")
   (set-background-color "#3F3F3F")
   (set-cursor-color     "#FFFFEF"))

(defun hcColorBW () "."
   (interactive)
   (set-foreground-color "#FFFFFF")
   (set-background-color "#000000")
   (set-cursor-color     "#FFFFFF"))

(defun hcHostedAppearance () "."
  (interactive)
  ;;(set-face-background 'default "grey")
  ;;(set-face-background 'default "gray19")
  ;;(set-face-background 'default "gray14")
  ;;(set-face-background 'default "gray10")
  (set-scroll-bar-mode 'right)
  ;;(set-face-font 'default "-unknown-DejaVu LGC Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (set-face-font 'default "-PfEd-TlwgTypewriter-normal-normal-normal-*-24-*-*-*-*-0-iso10646-1")
  ;;(hc-w 160)
  ;;(hc-h 43)
  )

;; type fc-list on command line to see fonts installed

;;(set-face-attribute 'default nil :family "courier" :height 140)
;;(set-face-font 'default "-*-Lucida Sans Typewriter-Medium-R-*-*-*-200-*-*-*-*-iso8859-1")
;;(set-face-font 'modeline "-*-Lucida Sans Typewriter-medium-R-*-*-*-200-*-*-*-*-iso8859-1")

;;; Background
;;(set-face-background 'default "#9900991b99fe") ; grey
;;(set-face-background 'default "#900060006000") ; earthy red
;;(set-face-background 'default "#de00b8008700") ; earthy orange
;;(set-face-background 'default "#737373737373") ; grey
;;(set-face-background 'default "#6a6a6a6a6a6a") ; grey
;;(set-face-background 'default "DarkSlateGrey")
;;(set-face-background 'default "grey")
;;(defined-colors)
;; ("snow" "ghost white" "GhostWhite" "white smoke" "WhiteSmoke" "gainsboro" "floral white" "FloralWhite" "old lace" "OldLace" "linen" "antique white" ...)
;;(set-face-background 'default "antique white")
;;(set-face-background 'default "grey99")
;;(set-face-background 'default "White")
;;(set-face-background 'default "#b9b9b9b9b9b9")
;;(set-face-background 'default "#dddddddddddd")
;;(set-face-background 'default "#68006f008200") ; blue
;;(set-face-background 'default "Black")

;;; Foreground
;;(set-face-foreground 'default "Green")
;;(set-face-foreground 'default "DarkSlateGrey")
;;(set-face-foreground 'default "#de00b8008700") ; earthy orange
;;(set-face-foreground 'default "Black")
;;(set-face-foreground 'default "white")

;;; Mark to region.
;;(set-face-background 'primary-selection "grey")
;;(set-face-foreground 'primary-selection "black")))

;;; Incremental search.
;;(set-face-foreground 'isearch "black")
;;(set-face-background 'isearch "green")))

;;; Toolbar.

;; Turn off font-lock?
(defvar font-lock-auto-fontify)
(defvar font-lock-mode-enable-list)
(defun hcFontLockModeHook () "."
  (if (fboundp 'global-font-lock-mode)
      (global-font-lock-mode -1) ;; Emacs
    (setq font-lock-auto-fontify nil))
  (setq font-lock-mode-enable-list nil))

(cond ((hcDarwinP)
       (hcMacFW)))

(provide 'hc-appearance)

;;; hc-appearance.el ends here
