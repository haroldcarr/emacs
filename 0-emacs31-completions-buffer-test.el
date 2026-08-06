;;; 0-emacs31-test.el --- default completions playground -*- lexical-binding: t -*-


;; Minibuffer completion (e.g., M-x, C-x C-f, C-x b) completing-read
;; - now not needed: Vertico, ivy and friends
;; In-buffer completion (completion-at-point)
;; - bound to M-TAB, and to TAB if set tab-always-indent to complete
;; - type in a buffer and the candidates come from completion-at-point-functions:
;;   - elisp symbols, file names, whatever your LSP server offers through Eglot.
;; - now not needed: Corfu and company handle this second case.

(defun my/minibuffer-truncate-lines ()
  "Keep minibuffer lines unwrapped."
  (setq truncate-lines t))

(defun my/flex-noinsert-try-completion (string table pred point)
  "Flex `try-completion' that never auto-extends the input on TAB.

The stock `flex' completion style does two jobs: it filters
candidates by fuzzy (subsequence) match, and its `try-completion'
merges the surviving candidates, inserting their common expansion
into the buffer.  With `tab-always-indent' set to `complete' that
merge means TAB silently types a candidate (often a far, wrong one)
*before* the *Completions* list is shown.  Eglot's own
`eglot--dumb-flex' avoids the merge but gives no relevance sorting.

This wrapper keeps flex's filtering and scoring but suppresses the merge:

  - no candidates           -> nil   (no match)
  - exactly one candidate   -> complete it fully
  - two or more candidates  -> return STRING unchanged, so TAB only
							   pops the *Completions* list.

STRING, TABLE, PRED and POINT are the usual `try-completion' args."
  (let ((all (completion-flex-all-completions string table pred point)))
	(cond
	 ((null all) nil)
	 ((= (safe-length all) 1)
	  (let ((sole (car all)))
		(if (string= sole string) t (cons sole (length sole)))))
	 (t (cons string point)))))

(use-package minibuffer
  :ensure nil
  :bind ( :map minibuffer-visible-completions-up-down-map
	  ("C-n" . minibuffer-next-completion)
	  ("C-p" . minibuffer-previous-completion))
  :hook ((minibuffer-setup . cursor-intangible-mode)
	 (minibuffer-setup . my/minibuffer-truncate-lines))
  :custom
  ;; TAB in a code buffer indents the line if it needs indenting, then completes if it doesn't.
  (tab-always-indent 'complete)

  ;; completion-auto-select t : moves point into the *Completions* window when TAB pops it up.
  ;; so you can navigate the list without a second key to get there.
  (completion-auto-help t)

  (completion-auto-select t)

  ;; completion-eager-display : whether *Completions* shows up on its own (default 'auto)
  ;; completion-eager-update : whether it refreshes as you type (default 'auto)
  ;; 'auto : only if completion table asks for it," but almost nothing asks.
  ;; t : get the completion buffer with TAB.
  (completion-eager-update t)
  (completion-eager-display t)

  (minibuffer-visible-completions 'up-down)
  (completion-ignore-case t)

  ;; completion-show-help nil drops the two-line
  ;; "Click or type RET on a completion to select it"
  ;; banner from the top of the buffer.
  (completion-show-help nil)

  ;; flex is fuzzy: matches an in-order subset of what you typed,
  ;; so faL finds faLinkedin and faList without you spelling out the middle.
  ;;
  ;; But flex does two jobs, and only one of them is filtering.
  ;; Its try-completion also merges the surviving candidates and inserts the result.
  ;; With tab-always-indent set to complete, that merge happens on TAB,
  ;; before you ever see the list.
  ;; But, supposed a page has a handful of FontAwesome icons and you want faLinkedin,
  ;; so I typed faL and pressed TAB once.  But no list is shown.
  ;; faEnvelopeSquare matches faL under flex because the l sits inside "Envelope",
  ;; and that's where the merge landed. TAB typed sixteen characters I didn't ask for,
  ;; hid the candidates I did want, and left me deleting.
  ;;
  ;; Eglot ran into this too and works around it with its own eglot--dumb-flex style,
  ;; which skips the merge. That fixes the typing and loses the scoring,
  ;; so candidates come back in whatever order the server sent them.
  ;; I wanted both: flex's filtering and relevance sorting,
  ;; and no insertion until I pick something. So I wrapped it with flex-noinsert (above)
  (completion-styles '(partial-completion flex initials))

  ;; A completion style in Emacs is four things in a list: a name, a try-completion function,
  ;; an all-completions function, and a doc string.
  ;; I reuse completion-flex-all-completions unchanged,
  ;; so filtering and scoring are stock flex, and I swap in a try-completion
  ;; that refuses to merge. The completion--adjust-metadata property
  ;; is what keeps *Completions* sorted by flex score rather than alphabetically.
  ;; Then aim it at Eglot only:
  (completion-category-overrides '((eglot-capf (styles flex-noinsert))))

  ;; completions-format 'one-column (vertical instead of horizontal list, default 'horizontal)
  ;; completions-max-height : max vertical height
  (completions-format 'one-column)
  (completions-max-height 10)

  ;; completions-sort 'historical sorts alphabetically,
  ;; but then puts whatever picked recently to the top.
  (completions-sort 'historical)

  ;; enable-recursive-minibuffers : enables starting a minibuffer command from inside another
  ;; minibuffer-depth-indicate-mode : puts a [2] in prompt so you know how deep you are.
  ;; minibuffer-electric-default-mode : hides the (default foo) part of a prompt
  ;;   as soon as you type, and brings it back if you erase.
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)

  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)

  ;; minibuffer-prompt-properties
  ;; - keep point out of the read-only prompt text,
  ;; - cursor-intangible-mode on minibuffer-setup-hook is what enforces it.
  ;; - truncate-lines in the minibuffer keeps long candidates on one line instead of ballooning the minibuffer to N rows.
  (minibuffer-prompt-properties
   '(read-only t
     intangible t
     cursor-intangible t
     face minibuffer-prompt))

  :config
  ;; Register the `flex-noinsert' style: same filtering/sorting as
  ;; `flex', but with the wrapper above as its try function.
  (add-to-list 'completion-styles-alist
	       '(flex-noinsert
		 my/flex-noinsert-try-completion
		 completion-flex-all-completions
		 "Flex matching that never extends input on TAB."))
  ;; Reuse flex's metadata tweak so *Completions* sorts by flex score.
  (put 'flex-noinsert 'completion--adjust-metadata
       'completion--flex-adjust-metadata))

;;; 0-emacs31-test.el ends here
