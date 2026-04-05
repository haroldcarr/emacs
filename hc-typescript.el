;;; hc-credo-commands.el --- completion                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hc/find-file-with-optional-at-signs ()
  "Open token at point as /path[:line[:col]], including @ in paths."
  (interactive)
  (let* ((start (save-excursion
                  (skip-chars-backward "^ \t\n\"'`()[]{}<>")
                  (point)))
         (end (save-excursion
                (skip-chars-forward "^ \t\n\"'`()[]{}<>")
                (point)))
         (raw (buffer-substring-no-properties start end))
         (parsed (hc/parse-path-line-col raw))
         (file (plist-get parsed :file))
         (line (plist-get parsed :line))
         (col (plist-get parsed :col)))
    (unless (and file (not (string-empty-p file)))
      (user-error "No path at point"))
    (find-file file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (when col
      (move-to-column (1- col)))))

;; optional keybinding
;; (global-set-key (kbd "C-c f") #'hc/find-file-with-optional-at-signs)

(defun hc/parse-path-line-col (input)
    "Parse INPUT as /path, /path:line, or /path:line:col.
  Returns plist: (:file STRING :line INT|nil :col INT|nil)."
    (let ((s (hc--trim-wrapping-punct (string-trim input))))
      (cond
       ((string-match "\\`\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\'" s)
        (list :file (match-string 1 s)
              :line (string-to-number (match-string 2 s))
              :col  (string-to-number (match-string 3 s))))
       ((string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" s)
        (list :file (match-string 1 s)
              :line (string-to-number (match-string 2 s))
              :col  nil))
       (t
        (list :file s :line nil :col nil)))))

(defun hc--trim-wrapping-punct (s)
    "Trim common leading/trailing wrappers around a copied path token."
    (let ((lead '(?\s ?\t ?\n ?\" ?\' ?\` ?\( ?\[ ?\{ ?<))
          (tail '(?\s ?\t ?\n ?\" ?\' ?\` ?\) ?\] ?\} ?> ?, ?\; ?.)))
      (while (and (> (length s) 0) (memq (aref s 0) lead))
        (setq s (substring s 1)))
      (while (and (> (length s) 0) (memq (aref s (1- (length s))) tail))
        (setq s (substring s 0 -1)))
      s))

(require 'ert)
(require 'cl-lib)

(ert-deftest hc/parse-path-line-col--pnpm-at-path ()
  (let* ((s "/Users/hcarr/ws/OLABS/github-mark-moir-credo-ts/node_modules/.pnpm/@sd-jwt+core@0.17.0/node_modules/@sd-jwt/core/src/index.ts:96")
         (r (hc/parse-path-line-col s)))
    (should (equal (plist-get r :file)
                   "/Users/hcarr/ws/OLABS/github-mark-moir-credo-ts/node_modules/.pnpm/@sd-jwt+core@0.17.0/node_modules/@sd-jwt/core/src/index.ts"))
    (should (= (plist-get r :line) 96))
    (should (null (plist-get r :col)))))

(ert-deftest hc/parse-path-line-col--line-col ()
  (let ((r (hc/parse-path-line-col "/tmp/a.ts:12:7")))
    (should (equal (plist-get r :file) "/tmp/a.ts"))
    (should (= (plist-get r :line) 12))
    (should (= (plist-get r :col) 7))))

(ert-deftest hc/parse-path-line-col--plain-path ()
  (let ((r (hc/parse-path-line-col "/tmp/a.ts")))
    (should (equal (plist-get r :file) "/tmp/a.ts"))
    (should (null (plist-get r :line)))
    (should (null (plist-get r :col)))))

(ert-deftest hc/parse-path-line-col--wrapped-punctuation ()
  (let ((r (hc/parse-path-line-col "('/tmp/a.ts:42',)")))
    (should (equal (plist-get r :file) "/tmp/a.ts"))
    (should (= (plist-get r :line) 42))))

(ert-deftest hc/find-file-with-optional-at-signs--opens-file-and-line-from-pnpm-token ()
  (let ((opened-file nil)
        (target-buffer (get-buffer-create " *hc-find-path-line-target*")))
    (with-current-buffer target-buffer
      (erase-buffer)
      (dotimes (i 200)
        (insert (format "line-%d\n" (1+ i)))))
    (unwind-protect
        (with-temp-buffer
          (insert "/Users/hcarr/ws/OLABS/github-mark-moir-credo-ts/node_modules/.pnpm/@sd-jwt+core@0.17.0/node_modules/@sd-jwt/core/src/index.ts:96")
          (goto-char (point-min))
          (search-forward "@sd-jwt+core")
          (cl-letf (((symbol-function 'find-file)
                     (lambda (file)
                       (setq opened-file file)
                       (switch-to-buffer target-buffer))))
            (hc/find-file-with-optional-at-signs))
          (should (equal opened-file
                         "/Users/hcarr/ws/OLABS/github-mark-moir-credo-ts/node_modules/.pnpm/@sd-jwt+core@0.17.0/node_modules/@sd-jwt/core/src/index.ts"))
          (should (= (line-number-at-pos) 96)))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

(ert-deftest hc/find-file-with-optional-at-signs--opens-file-line-and-col ()
  (let ((opened-file nil)
        (target-buffer (get-buffer-create " *hc-find-path-line-target*")))
    (with-current-buffer target-buffer
      (erase-buffer)
      (dotimes (i 50)
        (insert (format "0123456789 line-%d\n" (1+ i)))))
    (unwind-protect
        (with-temp-buffer
          (insert "(/tmp/a.ts:12:7)")
          (goto-char (point-min))
          (search-forward "a.ts")
          (cl-letf (((symbol-function 'find-file)
                     (lambda (file)
                       (setq opened-file file)
                       (switch-to-buffer target-buffer))))
            (hc/find-file-with-optional-at-signs))
          (should (equal opened-file "/tmp/a.ts"))
          (should (= (line-number-at-pos) 12))
          (should (= (current-column) 6)))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

;; (ert "^hc/\\(parse-path-line-col\\|find-path-line-at-point\\)--")
;; (ert "^hc/parse-path-line-col--")
;; (ert "^hc/find-file-with-optional-at-signs--")

(defun hc/run-credo-path-tests ()
  (interactive)
  (ert "^hc/\\(parse-path-line-col\\|find-file-with-optional-at-signs\\)--"))

(provide 'hc-credo-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - **What `*.d.mts` files are**
;;   - They are **TypeScript declaration files** for ESM modules (`.mts` world).
;;   - They contain **types only** (signatures, interfaces), not runtime code.
;;   - Runtime code is in `.mjs` / `.js`.

;; - **How to make Emacs + Eglot handle this well**
;;   1. Ensure `.d.mts` opens in TS mode:
;;      ```elisp
;;      (add-to-list 'auto-mode-alist '("\\.d\\.mts\\'" . typescript-ts-mode))
;;      ;; or typescript-mode if you use that major mode
;;      ```
;;   2. Ensure Eglot starts for TS buffers (usually automatic with `typescript-language-server` + `tsserver` installed).
;;   3. Use:
;;      - `M-.` for definition (usually lands in `.d.ts`/`.d.mts`)
;;      - `M-?` for references
;;   4. For runtime implementation, jump manually from declaration to `dist/index.mjs` (or grep for symbol), because many npm packages ship declarations without full source maps back to original `src/*.ts`.


;; ;; Jump from declaration symbol (e.g. in *.d.mts) to runtime impl in dist/index.mjs
;; (defun hc/ts-jump-decl-to-mjs ()
;;   "Jump from symbol at point in *.d.mts/*.d.ts to same symbol in sibling index.mjs."
;;   (interactive)
;;   (let* ((sym (thing-at-point 'symbol t))
;;          (file (buffer-file-name)))
;;     (unless (and sym file)
;;       (user-error "Need a file buffer with symbol at point"))
;;     (unless (string-match-p "\\.d\\.m?ts\\'" file)
;;       (user-error "Current file is not a declaration file (*.d.ts or *.d.mts)"))
;;     (let* ((dir (file-name-directory file))
;;            (mjs (expand-file-name "index.mjs" dir))
;;            (js  (expand-file-name "index.js" dir))
;;            (target (cond ((file-exists-p mjs) mjs)
;;                          ((file-exists-p js) js)
;;                          (t nil))))
;;       (unless target
;;         (user-error "No sibling index.mjs or index.js found"))
;;       (find-file target)
;;       ;; Try common function forms in transpiled output first, then fallback.
;;       (or (re-search-forward (format "^[[:space:]]*%s[[:space:]]*(" (regexp-quote sym)) nil t)
;;           (re-search-forward (format "function[[:space:]]+%s[[:space:]]*(" (regexp-quote sym)) nil t)
;;           (re-search-forward (format "\\_<%s\\_>" (regexp-quote sym)) nil t)
;;           (user-error "Symbol '%s' not found in %s" sym target))
;;       (beginning-of-line)
;;       (recenter))))

;; Optional keybind (TS buffers):

;; (with-eval-after-load 'typescript-ts-mode
;;   (define-key typescript-ts-mode-map (kbd "C-c C-j") #'hc/ts-jump-decl-to-mjs))

;; Use it with point on `issue` in `index.d.mts`, then `M-x hc/ts-jump-decl-to-mjs`.


;;; hc-credo-commands.el ends here
