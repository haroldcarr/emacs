;;; hc-web-browsing.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

;; http://www.howardism.org/Technical/Emacs/browsing-in-emacs.html

(use-package osx-browse)

(setq
  ;; See: http://ergoemacs.org/emacs/emacs_set_default_browser.html
  browse-url-handlers
  '(("haroldcarr\\.com"            . eww-browse-url)
    ;;("."                           . osx-browse-url-chrome)
    ("."                           . osx-browse-url) ;; use default
   ))

(defun hc-hoogle-eww (s)
  "."
  (interactive "sHoggle: ")
  (eww-browse-url (url-encode-url (concat "https://hoogle.haskell.org/?hoogle=" s))))

(defun hc-hoogle-chrome (s)
  "."
  (interactive "sHoggle: ")
  (osx-browse-url-chrome (url-encode-url (concat "https://hoogle.haskell.org/?hoogle=" s))))

;; ------------------------------------------------------------------------------
;; https://joshblais.com/blog/emacs-as-my-browser/

(setq eww-search-prefix "https://searx.labrynth.org/search?q=")
(setq eww-download-directory (expand-file-name "~/Downloads/"))

(defun my-browse-url-mpv (url &rest _args)
  "Open URL in mpv."
  (start-process "mpv" nil "mpv" url))

(defun my-browse-url-pdf (url &rest _args)
  "Fetch remote PDF and open in pdf-tools within Emacs."
  (let ((tmp (make-temp-file "emacs-pdf-" nil ".pdf")))
    (url-copy-file url tmp t)
    (find-file-other-window tmp)
    (pdf-view-mode)))

(setq browse-url-handlers
      '(("\\(youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\|twitch\\.tv\\)" . my-browse-url-mpv)
        ("\\.mp4$" . my-browse-url-mpv)
        ("\\.pdf$" . my-browse-url-pdf)
        ("^gemini://" . elpher-browse-url-elpher)
        ("^gopher://" . elpher-browse-url-elpher)
        ("." . eww-browse-url)))

;; ;; Keep your fallback setting
;; (setq browse-url-secondary-browser-function 'browse-url-generic
;;       browse-url-generic-program "chromium")

;; (setq shr-width 100)
;; (setq shr-max-width 120)
;; (setq shr-indentation 4)

;; (setq shr-use-fonts nil)
;; (setq shr-max-image-size '(800 . 600))
;; (setq shr-image-animate t)

(defun my/eww-download-image-at-point ()
  "Download image at point to `eww-download-directory'."
  (interactive)
  (let ((url (or (get-text-property (point) 'image-url)
                 (get-text-property (point) 'shr-url))))
    (if (not url)
        (message "No image at point")
      (let* ((filename (file-name-nondirectory (url-filename (url-generic-parse-url url))))
             (dest (expand-file-name filename eww-download-directory)))
        (url-copy-file url dest t)
        (message "Saved: %s" dest)))))

;; Keybinds
;; (with-eval-after-load 'eww
;;   (define-key eww-mode-map (kbd "b") #'eww-back-url)
;;   (define-key eww-mode-map (kbd "a") #'eww-add-bookmark)
;;   (define-key eww-mode-map (kbd "U") #'shr-copy-url)
;;   (define-key eww-mode-map (kbd "D") #'my/eww-download-image-at-point))

(provide 'browser)

(provide 'hc-web-browsing)

;;; hc-web-browsing.el ends here
