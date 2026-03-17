;;; hc-credo-commands.el --- completion                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hc/find-path-line-at-point ()
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
;; (global-set-key (kbd "C-c f") #'hc/find-path-line-at-point)

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

;; Run tests with: M-x ert RET "^hc/parse-path-line-col--" RET

;;(global-set-key (kbd "C-c f") #'hc/find-file-at-point-path-line)

(provide 'hc-credo-commands)

;;; hc-credo-commands.el ends here
