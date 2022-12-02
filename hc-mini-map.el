;;; hc-mini-map --- packages for showing miniture version of file as a sidebar

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/dengste/minimap/blob/master/minimap.el

(use-package minimap
  :ensure nil
  :config
  (custom-set-faces
   '(minimap-font-face ((t (:height 120 :family "DejaVu Sans Mono")))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/zk-phi/sublimity

;; (use-package sublimity-scroll     :ensure nil)
;; (use-package sublimity-map        :ensure nil)
;; (use-package sublimity-attractive :ensure nil)
(use-package sublimity
  :ensure nil
  :config
  (progn
    (setq sublimity-scroll-weight       10)
    (setq sublimity-scroll-drift-length  5)

    (setq sublimity-map-size       20)
    (setq sublimity-map-fraction    0.3)
    (setq sublimity-map-text-scale -7)

    (sublimity-map-set-delay 5)

    (add-hook 'sublimity-map-setup-hook
              (lambda ()
                (setq buffer-face-mode-face '(:family "Monospace"))
                (buffer-face-mode)))

    (setq sublimity-attractive-centering-width 110)

    ;;(sublimity-attractive-hide-bars)
    ;;(sublimity-attractive-hide-vertical-border)
    ;;(sublimity-attractive-hide-fringes)
    ;;(sublimity-attractive-hide-modelines)
  )
)

;; Enable via: (sublimity-mode 1)

(provide 'hc-bookmark-plus)

;;; hc-mini-map.el ends here

