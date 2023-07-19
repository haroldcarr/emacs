;;; hc-org-mode --- hc init Org Mode

;;; Commentary:

;; http://www.gnu.org/software/emacs/manual/html_node/org/index.html#Top
;; http://emacswiki.org/emacs/OrgMode
;; http://orgmode.org/org.html
;; - complete manaul in one HTML file
;; http://orgmode.org/manual/index.html

;;; Code:

(eval-when-compile (require 'use-package))

(defun hc-org-mode ()
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|rpt\\|txt\\)$" . org-mode))
  ;; So I can visit script links in org files (instead of execute them)
  ;; I link to the real file: ln -s <some-script> <some-script>.hcScript
  (add-to-list 'org-file-apps '("\\.hcScript\\'" . emacs))

  ;; "Standard" key bindings (but not provided)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  ;; TODO: find a replacement
  ;; \C-c & is the normal keybinding for org-mark-ring-goto, but it is taken by yasnippet 

  ;;;
  ;;; Org Misc
  ;;;

  ;; do not automatically indent below headlines
  (setq org-adapt-indentation nil)

  ;; If you do not like transient-mark-mode, you can create an active
  ;; region by using the mouse to select a region, or pressing C-<SPC>
  ;; twice before moving the cursor.

  ;;(setq org-hide-leading-stars t)

  ;; show the whole file when first visited
  (setq org-startup-folded 'showeverything)

  ;; don't fold when yanking
  (setq org-yank-folded-subtrees nil)

  ;; Org buffers only
  ;;(add-hook 'org-mode-hook 'turn-on-font-lock)

  ;; The default is 3
  (setq org-export-headline-levels 6)

  ;; Do not put the validate link at bottom of page
  (setq org-export-html-validation-link nil) ; I think this is obsolete.
  (setq org-html-postamble-format nil)       ; I think this is the replacement - does not seem to work.

  ;; Do not put timestamp at bottom of page
  (setq org-export-time-stamp-file nil)

  ;; Do not put author at bottom of page
  (setq org-export-author-info nil)

  ;; Do not put in validation link in HTML export
  (setq org-html-validation-link nil)

  ;; let ME control org-mode font colors, etc.
  (setq org-export-htmlize-output-type 'css)

  ;;;
  ;;; Agenda
  ;;;

  ;; Include entries from the emacs diary into =org-mode='s agenda.
  (setq org-agenda-include-diary t)

  ;; org-mode manages the =org-agenda-files= variable automatically using
  ;; C-c [ and C-c ] to add/remove files respectively.
  ;; Instead, disable those keys and replace with an explicit directory list.
  ;; Any org files in those directories are automatically included in the agenda.
  (setq org-agenda-files
        (list
         (concat (hcRpt)      "/TODO-WORK.org")
         (concat (hcFinance)  "/01-TODO.org")
         (concat (hcFsync)    "/TODO-ME.org")
         ;; (hcRpt)
         ;; (concat (hcRpt)   "/.past/2014")
         ;; (concat (hcRpt)   "/.past/2013")
         ;; (concat (hcRpt)   "/.past/2012")
         ;; (concat (hcRpt)   "/.past/2011")
         ;; "/tmp/google.org"
         ))

  (add-hook 'org-mode-hook
            (lambda ()
              (org-defkey org-mode-map "\C-c["    'undefined)
              (org-defkey org-mode-map "\C-c]"    'undefined)))

  (setq org-highest-priority ?A)
  (setq org-lowest-priority  ?E)
  (setq org-default-priority ?E)

  (setq org-todo-keywords
        '((sequence "NEXT(n)" "INPG(i)" "TODO(t)" "SUSP(s)" "|" "DONE(d!/!)" "DROP(D!/!)")
          ))

  (setq org-todo-keyword-faces
        '(("NEXT"       :foreground "white"          :weight bold)
          ("INPG"       :foreground "orange"         :weight bold) -- in progress
          ("TODO"       :foreground "yellow"         :weight bold)
          ("SUSP"       :foreground "magenta"        :weight bold) -- suspended
          ("DONE"       :foreground "forest green"   :weight bold)
          ("DROP"       :foreground "brown"          :weight bold)
          ))

  ;;;
  ;;; Literate programming
  ;;;

  ;; Important: set this or it will remove space after editing code: C-c,C-c,'
  ;; The default is 2.
  (setq org-edit-src-content-indentation 4)

  ;; When exporting code I want it to look like what I wrote.
  (setq org-src-preserve-indentation t)

  ;; When editing code, use the current window.
  (setq org-src-window-setup (quote current-window))

  ;;; see http://doc.norang.ca/org-mode.html
  ;;; see http://home.fnal.gov/~neilsen/notebook/orgExamples/org-examples.html

  ;; https://github.com/stathissideris/ditaa (old http://ditaa.org/ditaa/)
  ;; jar comes with org-mode in contrib/scripts (but override to ensure latest)
  (setq org-ditaa-jar-path    (concat (hcUlhcd) "/java/ditaa/ditaa.jar"))

  ;; http://plantuml.sourceforge.net/
  (setq org-plantuml-jar-path (concat (hcUlhcd) "/java/plantuml/plantuml.jar"))

  (add-hook 'org-babel-after-execute-hook 'hc/display-inline-images 'append)

  ;; Make babel results blocks lowercase
  (setq org-babel-results-keyword "results")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa      . t)
     (dot        . t)
     (emacs-lisp . t)
     (gnuplot    . t)
     (haskell    . t)
     (latex      . t)
     (plantuml   . t)
     ))

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)
  ;(setq org-confirm-babel-evaluate t)

  ;; Cache all babel results blocks by default
  ;; For graphics generation, this is faster if nothing changes
  (if (and (not (hcXEmacsP))
           (fboundp 'org-babel-default-header-args))
      (setq org-babel-default-header-args
            (cons '(:cache . "yes")
                  (assq-delete-all :cache org-babel-default-header-args))))
)

(defun hc/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(use-package ox-md :defer t) ;; load this so menu options show up

(use-package ox-beamer :defer t) ;; see: http://nickhigham.wordpress.com/2013/07/05/emacs-org-mode-version-8/

(provide 'hc-org-mode)

;;; hc-org-mode.el ends here
