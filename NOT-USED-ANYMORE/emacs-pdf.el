;;; -*- lexical-binding: t -*-
;;;
;;; emacs-pdf.el
;;;
;;; Print buffer to PDF file.
;;;
;;; Copyright (C) 2020 Tomas Hlavaty <tom at logand dot com>
;;;
;;; License: GPLv3 or later
;;;
;;; Download: git clone https://logand.com/git/emacs-pdf.git
;;;
;;; Example configuration:
;;;
;;; (add-to-list 'load-path "~/git/emacs-pdf")
;;; (require 'emacs-pdf)
;;;
;;; Example usage:
;;;
;;; M-x pdf-buffer or pdf-region in example.txt will create
;;; example.txt.pdf file.
;;;
;;; Some variables can be customized in pdf and ps groups.

(require 'cl)
(require 'printing)

(defcustom pdf-line-height-factor 1.2
  "Specify the line height factor (line-height = factor * font-size)."
  :type 'number
  :group 'pdf)

(defcustom pdf-base-font '/Courier
  "PDF base font."
  :type '(choice
          (const /Courier)
          (const /Courier-Bold)
          (const /Courier-Oblique)
          (const /Courier-BoldOblique)
          (const /Helvetica)
          (const /Helvetica-Bold)
          (const /Helvetica-Oblique)
          (const /Helvetica-BoldOblique)
          (const /Times-Roman)
          (const /Times-Bold)
          (const /Times-Italic)
          (const /Times-BoldItalic))
  :group 'pdf)

(defcustom pdf-header
  (list 'pdf-page-number "/" 'pdf-number-of-pages " " 'pdf-nondirpart)
  "The items to display in the page header.

The value should be a list of strings and symbols.  Strings are
inserted unchanged.  For symbols with bound functions, the
function is called and should return a string to be inserted.
For symbols with bound values, the value should be a string to be
inserted.  If symbols are unbounded, they are silently ignored.

Useful functions:
- pdf-dirpart
- pdf-iso8601-date
- pdf-nondirpart
- pdf-number-of-pages
- pdf-page-number
"
  :type '(repeat (choice :menu-tag "Header"
			 :tag "Header"
			 string symbol))
  :group 'pdf)

(defcustom pdf-footer
  (list 'pdf-iso8601-date " " 'pdf-dirpart)
  "The items to display in the page footer.

The value should be a list of strings and symbols.  Strings are
inserted unchanged.  For symbols with bound functions, the
function is called and should return a string to be inserted.
For symbols with bound values, the value should be a string to be
inserted.  If symbols are unbounded, they are silently ignored.

Useful functions:
- pdf-dirpart
- pdf-iso8601-date
- pdf-nondirpart
- pdf-number-of-pages
- pdf-page-number
"
  :type '(repeat (choice :menu-tag "Footer"
			 :tag "Footer"
			 string symbol))
  :group 'pdf)

(defcustom pdf-end-regexp nil
  "Specify regexp which ends the printable part of the file.

As an example, it may be set to \"^Local Variables:\", in order to leave out
some special printing instructions from the actual print.  Special printing
instructions may be appended to the end of the file just like any other
buffer-local variables.  See section \"Local Variables in Files\" on Emacs
manual for more information.

It controls what actually gets printed and may be set to nil in which case
the end of the file ends the printable region."
  :type '(choice (const :tag "No Delimiter" nil)
		 (regexp :tag "Delimiter Regexp"))
  :group 'pdf)

;; for eldoc
(defun pdf (&rest rest) `(pdf ,@rest))
(defun pdf-xref (&rest kv) `(pdf-xref ,@kv))
(defun pdf-stream (dic &rest rest) `(pdf-stream ,dic ,@rest))
(defun pdf-obj (num ver one) `(pdf-obj ,num ,ver ,one))
(defun pdf-dic (&rest kv) `(pdf-dic ,@kv))
(defun pdf-vec (&rest rest) `(pdf-vec ,@rest))
(defun pdf-ref (num ver) `(pdf-ref ,num ,ver))

(defun insert-pdf (x)
  "Serialize cons tree x as PDF octets into the current buffer."
  (let (objs xrefpos)
    (cl-labels ((rec (x)
                     (etypecase x
                       (integer
                        (insert (format "\n%d" x)))
                       (symbol
                        (insert (format "\n%s" x)))
                       (string
                        (insert "\n(")
                        (loop
                         for c across x
                         do (case c
                              (?\\ (insert "\\\\"))
                              (?\( (insert "\\("))
                              (?\) (insert "\\)"))
                              (t (insert c))))
                        (insert ")"))
                       (real
                        (insert (format "\n%f" x)))
                       (cons
                        (ecase (car x)
                          (pdf
                           (insert "%PDF-1.4")
                           (insert
                            (format "\n%%%c%c%c%c%c%c%c%c"
                                    150 151 152 153 255 254 253 252))
                           (mapc #'rec (cdr x))
                           (insert "\nstartxref")
                           (insert (format "\n%d" xrefpos))
                           (insert "\n%%EOF\n"))
                          (pdf-xref
                           (setq xrefpos (point))
                           (insert "\nxref")
                           (insert (format "\n%d %d" 0 (1+ (length objs))))
                           (insert "\n0000000000 65535 f\r")
                           (dolist (obj (setq objs (nreverse objs)))
                             (destructuring-bind (a b pos &rest c) obj
                               (ignore a)
                               (insert (format "\n%010d %05d n\r" pos b))))
                           (insert "\ntrailer")
                           (rec
                            `(pdf-dic /Size ,(1+ (length objs)) ,@(cdr x))))
                          (pdf-stream
                           (destructuring-bind (a &rest b) (cdr x)
                             (let ((x (with-temp-buffer
                                        (set-buffer-multibyte nil)
                                        (mapc #'rec b)
                                        (buffer-string))))
                               (rec (if a
                                        `(/Length ,(length x) ,@a)
                                      `(pdf-dic /Length ,(length x))))
                               (insert "\nstream")
                               (insert x)
                               (insert "\nendstream"))))
                          (pdf-obj
                           (destructuring-bind (a b &rest c) (cdr x)
                             (push `(,a ,b ,(point)) objs)
                             (insert (format "\n%d %d obj" a b))
                             (mapc #'rec c)
                             (insert "\nendobj")))
                          (pdf-dic
                           (insert "\n<<")
                           (mapc #'rec (cdr x))
                           (insert "\n>>"))
                          (pdf-vec
                           (insert "\n[")
                           (mapc #'rec (cdr x))
                           (insert "\n]"))
                          (pdf-ref
                           (destructuring-bind (a b) (cdr x)
                             (insert (format "\n%d %d R" a b)))))))))
      (set-buffer-multibyte nil)
      (rec x))))

(defun pdf-brook-collect (brook)
  "Collect all values pulled from brook."
  (loop
   with z = nil
   while (setq z (funcall brook))
   collect z))

(defun pdf-brook-appending (brook)
  "Append all values pulled from brook."
  (loop
   with z = nil
   while (setq z (funcall brook))
   appending z))

(defun pdf-brook-count (brook)
  "Count all values in brook."
  (loop
   with z = nil
   while (setq z (funcall brook))
   count z))

(defun pdf-brook (x)
  "Make new brook from x."
  (etypecase x
    (list
     (lambda ()
       (pop x)))))

;;(pdf-brook-collect (pdf-brook '(1 2 3 4)))
;;(pdf-brook-count (pdf-brook '(1 2 3 4)))

(defun pdf-flat-brook (&rest brooks)
  "Compose brooks left to right, depth-first."
  (lambda ()
    (block loop
      (while brooks
        (let ((z (funcall (car brooks))))
          (cond
           ((functionp z) (push z brooks))
           (z (return-from loop z))
           (t (pop brooks))))))))

;;(pdf-brook-collect (pdf-flat-brook (pdf-brook '(1 2 3)) (pdf-brook '(4 5 6))))

(defun pdf-source-line-brook (string)
  "Make brook from string.  Elements are string per line or 'pagebreak."
  (pdf-flat-brook
   (let ((pages (pdf-brook (split-string string ""))))
     (lambda ()
       (let ((page (funcall pages)))
         (when page
           (pdf-brook (list (pdf-brook (split-string page "\n"))
                            (pdf-brook '(pagebreak))))))))))

;;(pdf-brook-collect (pdf-source-line-brook "1\n23\n4\n5"))

(defun pdf-source-page-brook (brook)
  "Make brook for single page from brook."
  (lambda ()
    (let ((z (funcall brook)))
      (when z
        (unless (eq 'pagebreak z)
          z)))))

;;(pdf-brook-collect (pdf-source-page-brook (pdf-source-line-brook "1\n23\n4\n5")))

(defvar *pdf-page-number*)
(defun pdf-page-number ()
  "Return current page number as string.  Useful in document
header or footer."
  (format "%s" *pdf-page-number*))

(defvar *pdf-number-of-pages*)
(defun pdf-number-of-pages ()
  "Return number of pages as string.  Useful in document header
or footer."
  (format "%s" *pdf-number-of-pages*))

(defvar *pdf-file-name*)
(defun pdf-dirpart ()
  "Return directory part as string.  Useful in document header or
footer."
  (and *pdf-file-name* (file-name-directory *pdf-file-name*)))
(defun pdf-nondirpart ()
  "Return non-directory part as string.  Useful in document header or
footer."
  (and *pdf-file-name* (file-name-nondirectory *pdf-file-name*)))

(defun pdf-iso8601-date ()
  "Return current data as ISO8601 string.  Useful in document
header or footer."
  (format-time-string "%Y-%m-%d"))

(defun pdf-header-or-footer-text (i n file-name list)
  "Insert document header or footer specified in list."
  (with-temp-buffer
    (let ((*pdf-page-number* i)
          (*pdf-number-of-pages* n)
          (*pdf-file-name* file-name))
      (dolist (x list)
        (when x
          (insert
           (etypecase x
             (function (or (funcall x) ""))
             (symbol (or (symbol-value x) ""))
             (string x))))))
    (buffer-string)))

(defun pdf-header-text (i n file-name)
  "Insert document header."
  (pdf-header-or-footer-text i n file-name pdf-header))

(defun pdf-footer-text (i n file-name)
  "Insert document footer."
  (pdf-header-or-footer-text i n file-name pdf-footer))

(defun pdf-line (x y line)
  "Represent PDF line as list of PDF drawing primitives."
  `(1 0 0 1 ,x ,y Tm ,line Tj))

(defun pdf-page-text (lines x0 y0 line-height header footer)
  "Represent PDF page as list of PDF drawing primitives."
  (pdf-brook-appending
   (let* ((page (pdf-source-page-brook lines))
          (bottom-margin (+ ps-bottom-margin
                            (if footer (+ line-height ps-footer-offset) 0)))
          (y y0))
     (lambda ()
       (when (<= bottom-margin (decf y line-height))
         (let ((line (funcall page)))
           (when line
             `(,@(when header
                   (prog1 (pdf-line x0 y header)
                     (setq header nil)
                     (decf y (+ line-height ps-header-offset))))
               ,@(when footer
                   (prog1 (pdf-line x0 ps-bottom-margin footer)
                     (setq footer nil)))
               ,@(pdf-line x0 y line)))))))))

(defun pdf-page-dimensions ()
  "Return values of page width and height depending on
ps-paper-type and ps-landscape-mode."
  (let ((x (cdr (assq ps-paper-type ps-page-dimensions-database))))
    (if ps-landscape-mode
        (values (cadr x) (car x))
      (values (car x) (cadr x)))))

;;(pdf-page-dimensions)

(defun pdf-pages-brook (substring x0 y0 line-height font-size
                                  npages file-name parent oid !ref)
  "Make brook of PDF objects per page."
  (let ((lines (pdf-source-line-brook substring))
        (i 0))
    (lambda ()
      (incf i)
      (let ((text (pdf-page-text lines x0 y0 line-height
                                 (when ps-print-header
                                   (pdf-header-text i npages file-name))
                                 (when ps-print-footer
                                   (pdf-footer-text i npages file-name)))))
        (when text
          (let ((oid1 (funcall oid))
                (oid2 (funcall oid)))
            (funcall !ref `(pdf-ref ,oid2 0))
            `((pdf-obj ,oid1
                       0
                       (pdf-stream nil
                                   BT
                                   /F1 ,font-size Tf
                                   1 0 0 1 ,x0 ,y0 Tm
                                   ,@text
                                   ET))
              (pdf-obj ,oid2
                       0
                       (pdf-dic /Type /Page
                                /Parent ,parent
                                /Resources (pdf-ref 2 0)
                                /Contents (pdf-ref ,oid1 0))))))))))

(defun pdf-npages (substring x0 y0 line-height font-size file-name)
  "Count number of pages in the document."
  (pdf-brook-count
   (pdf-pages-brook substring x0 y0 line-height font-size 0 file-name nil
                    (lambda () 0)
                    (lambda (x) (ignore x)))))

(defun pdf-region (from to &optional file-name)
  "Save region to PDF file."
  (interactive "r")
  (multiple-value-bind (page-width page-height) (pdf-page-dimensions)
    (let* ((coding-system-for-write 'raw-text-unix)
           (source-file-name (or file-name
                                 (buffer-file-name)
                                 (buffer-name)))
           (file-name (concat source-file-name ".pdf"))
           (x0 ps-left-margin)
           (y0 (- page-height ps-top-margin))
           (font-size (etypecase ps-font-size
                        (number ps-font-size)
                        (cons (if ps-landscape-mode
                                  (car ps-font-size)
                                (cdr ps-font-size)))))
           (line-height (* pdf-line-height-factor font-size))
           (substring (buffer-substring-no-properties
		       from
		       (or (when pdf-end-regexp
			     (save-excursion
			       (goto-char (point-min))
			       (when (re-search-forward pdf-end-regexp to 'noerror)
				 (min to (match-beginning 0)))))
			   to)))
           (npages (when (or (when ps-print-header
                               (member 'pdf-number-of-pages pdf-header))
                             (when ps-print-footer
                               (member 'pdf-number-of-pages pdf-footer)))
                     (pdf-npages substring x0 y0 line-height font-size
                                 source-file-name))))
      (with-temp-buffer
        (insert-pdf
         (let ((oid 0)
               %pages
               (%parent `(pdf-ref -1 0)))
           `(pdf
             (pdf-obj ,(incf oid)
                      0
                      (pdf-dic /Type /Font
                               /Subtype /Type1
                               /Name /F1
                               /BaseFont ,pdf-base-font))
             (pdf-obj ,(incf oid)
                      0
                      (pdf-dic /ProcSet (pdf-vec /PDF /Text)
                               /Font (pdf-dic /F1 (pdf-ref ,(1- oid) 0))))
             ,@(pdf-brook-appending
                (pdf-pages-brook substring x0 y0 line-height font-size npages
                                 source-file-name
                                 %parent
                                 (lambda () (incf oid))
                                 (lambda (x) (push x %pages))))
             (pdf-obj ,(setf (cadr %parent) (incf oid))
                      0
                      (pdf-dic /Type /Pages
                               /Count ,(length %pages)
                               /Kids (pdf-vec ,@(nreverse %pages))
                               /MediaBox (pdf-vec 0 0 ,page-width ,page-height)))
             (pdf-obj ,(incf oid)
                      0
                      (pdf-dic /Type /Catalog
                               /Pages (pdf-ref ,(1- oid) 0)))
             (pdf-xref /Root (pdf-ref ,oid 0)))))
        (write-region (point-min) (point-max) file-name)))))

(defun pdf-buffer (&optional file-name)
  "Save buffer to PDF file."
  (interactive)
  (pdf-region (point-min) (point-max) file-name))

(provide 'emacs-pdf)
