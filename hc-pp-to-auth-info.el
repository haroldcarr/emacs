;;; hc-pp-to-auth-info.el --- xx          -*- lexical-binding: t; -*-

;;; Commentary:

;; https://www.gnu.org/software/inetutils/manual/html_node/The-_002enetrc-file.html
;; name must be labeled machine
;; but when searching, use :host (auth-source-search :host "dropbox")

;; TODO: if there are double quotes inside field values, it breaks when parsing/creating value-lines
;; TODO: if there are commas        inside field values, it breaks when splitting value-lines via comma

;; https://systemcrafters.net/emacs-tips/using-encrypted-passwords/
;; TODO: .gnupg in home

;;; Code:

(defun hc-pp-to-auth-info (infile outfile)
  (hc-write-authinfo-lines-to-file
   outfile
   (hc-pp-value-line-to-authinfo-lines
    (hc-parse-collect-pp-value-lines infile))))

(defun hc-write-authinfo-lines-to-file (outfile lines)
  (with-temp-file outfile
    (insert (mapconcat 'identity lines "\n"))))

(defun hc-parse-collect-pp-value-lines (infile)
  (with-temp-buffer
    (insert-file-contents infile)
    (goto-char (point-min))
    (let ((result nil))
      (while (progn (print (eobp)) (not (eobp)))
        (let ((r (hc-get-value-line (point))))
          (print "----------------------")
          (print r)
          (setq result (cons r result))
          (hc-forward-to-next-value-line-or-eob)))
      (reverse result))))

(defun hc-get-value-line (start)
  (let ((count 0))
    (while (< count 14)
      (let ((c (char-after)))
        (if (char-equal c ?\")
            (setq count (+ count 1))))
      (forward-char))
    (buffer-substring start (point))))

(defun hc-forward-to-next-value-line-or-eob ()
  (forward-char)
  (while (and (not (eobp)) (not (char-equal (char-after) ?\")))
    (forward-char)))

(defun hc-pp-value-line-to-authinfo-lines (pp-value-lines)
  (mapcar #'(lambda (pp-value-line) (hc-pp-value-line-to-authinfo-line pp-value-line))
          pp-value-lines))

(defun hc-pp-value-line-to-authinfo-line (pp-value-line)
  "Convert a single PP-VALUE-LINE from passpack to .authinfo format.
Handles multiline myNotes and sharedNotes.
The line may have values for the following fields:
\"name\",\"login\",\"password\",\"website\",\"email\",\"myNotes\",\"sharedNotes\"
Returns an .authinfo formatted string."
  (let ((fields (split-string pp-value-line "," nil "\"")))
    (if (not (= (length fields) 7))
        (error "should have 7 fields, but has %d, fields are %S" (length fields) fields)
      (let ((name         (nth 0 fields))
            (login        (nth 1 fields))
            (password     (nth 2 fields))
            (website      (nth 3 fields))
            (email        (nth 4 fields))
            (my-notes     (nth 5 fields))
            (shared-notes (nth 6 fields)))
        (format "%s %s %s %s %s %s %s"
                (hc-maybeField "machine"     name)        ;; <------ MACHINE
                (hc-maybeField "login"       login)
                (hc-maybeField "password"    password)
                (hc-maybeField "website"     website)
                (hc-maybeField "email"       email)
                (hc-maybeField "myNotes"     my-notes)
                (hc-maybeField "sharedNotes" shared-notes))))))


(defun hc-maybeField (name value)
  (if (equal value "")
      ""
    (concat name " " "\"" value "\"")))

(provide 'hc-pp-to-auth-info)

;;; hc-pp-to-auth-info.el ends here


