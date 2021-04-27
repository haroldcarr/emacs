(require 'shr)
(require 'eww)
(require 'prot-eww)

(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

;(prot-emacs-builtin-package 'browse-url
;  (setq browse-url-browser-function 'eww-browse-url)
;  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(prot-emacs-builtin-package 'shr
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width nil)                  ; check `prot-eww-readable'
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(prot-emacs-builtin-package 'eww
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%t <%u>")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory
        (thread-last user-emacs-directory (expand-file-name "eww-bookmarks/")))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil)

  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
  ;;HC(define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill)) ; same

(prot-emacs-builtin-package 'prot-eww
  (define-prefix-command 'prot-eww-map)
  (define-key global-map (kbd "C-c w") 'prot-eww-map)
  (define-key global-map (kbd "s-w") 'prot-eww-map)
  (let ((map prot-eww-map))
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "a") #'prot-eww-search-arch-wiki)
    (define-key map (kbd "A") #'prot-eww-search-arch-aur)
    (define-key map (kbd "d") #'prot-eww-search-debbugs)
    (define-key map (kbd "w") #'prot-eww-search-wikipedia)
    (define-key map (kbd "s") #'prot-eww-search-engine))
  (let ((map eww-mode-map))
    (define-key map (kbd "B") #'prot-eww-bookmark-page)
    (define-key map (kbd "D") #'prot-eww-download-html)
    (define-key map (kbd "F") #'prot-eww-find-feed)
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "E") #'prot-eww-visit-url-on-page)
    (define-key map (kbd "J") #'prot-eww-jump-to-url-on-page)
    (define-key map (kbd "R") #'prot-eww-readable)))

(provide 'prot-eww-key-bindings)
