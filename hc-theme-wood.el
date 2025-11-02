;;; https://github.com/ed9w2in6/wood-theme?tab=readme-ov-file

(use-package autothemer
 :straight (autothemer
   :type git
   :host github
   :repo "jasonm23/autothemer"))
(use-package wood-theme
  :straight (wood-theme
    :type git
    :host github
    :repo "ed9w2in6/wood-theme")
  :after autothemer
  :init
  ;; Change these accordingly, MUST be evaluated before load-theme
  (setq wood-tab-line-box-line-width (if (display-graphic-p)
                                         (/ (max 2 (line-pixel-height)) 2)
                                         -1)
        wood-default-face-height 180
        wood-default-face-font-family "VictorMono Nerd Font Mono"
        wood-variable-pitch-face-font-family "Source Sans Pro"
        )
  :config (load-theme 'wood t))
