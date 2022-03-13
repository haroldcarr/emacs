;;; mel-b.el: Base64 encoder/decoder for GNU Emacs

;; Copyright (C) 1992,1995,1996,1997 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/6/24
;; Version: $Id: mel-b.el,v 1.2 2002/03/14 02:22:29 carr Exp $
;; Keywords: MIME, Base64

;; This file is part of MEL (MIME Encoding Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;; I got this from XEmacs 20.4
;;(require 'emu)


;;; @ variables
;;;

(defvar base64-external-encoder '("mmencode")
  "*list of base64 encoder program name and its arguments.")

(defvar base64-external-decoder '("mmencode" "-u")
  "*list of base64 decoder program name and its arguments.")

(defvar base64-internal-encoding-limit 1000
  "*limit size to use internal base64 encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defvar base64-internal-decoding-limit 1000
  "*limit size to use internal base64 decoder.
If size of input to decode is larger than this limit,
external decoder is called.")


;;; @ internal base64 decoder/encoder
;;;	based on base64 decoder by Enami Tsugutomo

;;; @@ convert from/to base64 char
;;;

(defun base64-num-to-char (n)
  (cond ((eq n nil) ?=)
	((< n 26) (+ ?A n))
	((< n 52) (+ ?a (- n 26)))
	((< n 62) (+ ?0 (- n 52)))
	((= n 62) ?+)
	((= n 63) ?/)
	(t (error "not a base64 integer %d" n))))

(defun base64-char-to-num (c)
  (cond ((and (<= ?A c) (<= c ?Z)) (- c ?A))
	((and (<= ?a c) (<= c ?z)) (+ (- c ?a) 26))
	((and (<= ?0 c) (<= c ?9)) (+ (- c ?0) 52))
	((= c ?+) 62)
	((= c ?/) 63)
	((= c ?=) nil)
	(t (error "not a base64 character %c" c))))


;;; @@ encode/decode one base64 unit
;;;

(defun base64-encode-1 (pack)
  (let ((a (car pack))
	(b (nth 1 pack))
	(c (nth 2 pack)))
    (concat
     (char-to-string (base64-num-to-char (ash a -2)))
     (if b
	 (concat
	  (char-to-string
	   (base64-num-to-char (logior (ash (logand a 3) 4) (ash b -4))))
	  (if c
	      (concat
	       (char-to-string
		(base64-num-to-char (logior (ash (logand b 15) 2) (ash c -6))))
	       (char-to-string (base64-num-to-char (logand c 63)))
	       )
	    (concat (char-to-string
		     (base64-num-to-char (ash (logand b 15) 2))) "=")
	    ))
       (concat (char-to-string
		(base64-num-to-char (ash (logand a 3) 4))) "==")
       ))))

(defun base64-decode-1 (pack)
  (let ((a (base64-char-to-num (car pack)))
	(b (base64-char-to-num (nth 1 pack)))
	(c (nth 2 pack))
	(d (nth 3 pack)))
    (concat (char-to-string (logior (ash a 2) (ash b -4)))
	    (if (and c (setq c (base64-char-to-num c)))
		(concat (char-to-string
			 (logior (ash (logand b 15) 4) (ash c -2)))
			(if (and d (setq d (base64-char-to-num d)))
			    (char-to-string (logior (ash (logand c 3) 6) d))
			  ))))))


;;; @@ base64 encoder/decoder for string
;;;

(defun base64-encode-string (string)
  "Encode STRING to base64, and return the result."
  (let ((len (length string))
	(b 0)(e 57)
	dest)
    (while (< e len)
      (setq dest
	    (concat dest
		    (mapconcat
		     (function base64-encode-1)
		     (pack-sequence (substring string b e) 3)
		     "")
		    "\n"))
      (setq b e
	    e (+ e 57)
	    )
      )
    (let* ((es (mapconcat
		(function base64-encode-1)
		(pack-sequence (substring string b) 3)
		""))
	   (m (mod (length es) 4))
	   )
      (concat dest es (cond ((= m 3) "=")
			    ((= m 2) "==")
			    ))
      )))

(defun base64-decode-string (string)
  "Decode STRING which is encoded in base64, and return the result."
  (mapconcat (function base64-decode-1)
	     (pack-sequence string 4)
	     ""))


;;; @ base64 encoder/decoder for region
;;;

(defun base64-internal-encode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((str (buffer-substring beg end)))
	(delete-region beg end)
	(insert (base64-encode-string str))
	)
      (or (bolp)
	  (insert "\n")
	  )
      )))

(defun base64-internal-decode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (looking-at ".*\n")
	(condition-case err
	    (replace-match
	     (base64-decode-string
	      (buffer-substring (match-beginning 0) (1- (match-end 0))))
	     t t)
	  (error
	   (prog1
	       (message (nth 1 err))
	     (replace-match "")))))
      (if (looking-at ".*$")
	  (condition-case err
	      (replace-match
	       (base64-decode-string
		(buffer-substring (match-beginning 0) (match-end 0)))
	       t t)
	    (error
	     (prog1
		 (message (nth 1 err))
	       (replace-match "")))
	    ))
      )))

(defun base64-external-encode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (as-binary-process (apply (function call-process-region)
				beg end (car base64-external-encoder)
				t t nil (cdr base64-external-encoder))
			 )
      ;; for OS/2
      ;;   regularize line break code
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match "")
	)
      )))

(defun base64-external-decode-region (beg end)
  (save-excursion
    (as-binary-process (apply (function call-process-region)
			      beg end (car base64-external-decoder)
			      t t nil (cdr base64-external-decoder))
		       )))

(defun base64-encode-region (start end)
  "Encode current region by base64.
START and END are buffer positions.
This function calls internal base64 encoder if size of region is
smaller than `base64-internal-encoding-limit', otherwise it calls
external base64 encoder specified by `base64-external-encoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "r")
  (if nil ;(and base64-internal-encoding-limit
	  ;     (> (- end start) base64-internal-encoding-limit))
      (base64-external-encode-region start end)
    (base64-internal-encode-region start end)
    ))

(defun base64-decode-region (start end)
  "Decode current region by base64.
START and END are buffer positions.
This function calls internal base64 decoder if size of region is
smaller than `base64-internal-decoding-limit', otherwise it calls
external base64 decoder specified by `base64-external-decoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "r")
  (if nil ;(and base64-internal-decoding-limit
	  ;     (> (- end start) base64-internal-decoding-limit))
      (base64-external-decode-region start end)
    (base64-internal-decode-region start end)
    ))


;;; @ base64 encoder/decoder for file
;;;

(defun base64-insert-encoded-file (filename)
  "Encode contents of file FILENAME to base64, and insert the result.
It calls external base64 encoder specified by
`base64-external-encoder'.  So you must install the program (maybe
mmencode included in metamail or XEmacs package)."
  (interactive (list (read-file-name "Insert encoded file: ")))
  (apply (function call-process) (car base64-external-encoder)
	 filename t nil (cdr base64-external-encoder))
  )


;;; @ etc
;;;

(defun base64-encoded-length (string)
  (let ((len (length string)))
    (* (+ (/ len 3)
	  (if (= (mod len 3) 0) 0 1)
	  ) 4)
    ))

(defun pack-sequence (seq size)
  "Split sequence SEQ into SIZE elements packs,
and return list of packs. [mel-b; tl-seq function]"
  (let ((len (length seq)) (p 0) obj
	unit (i 0)
	dest)
    (while (< p len)
      (setq obj (elt seq p))
      (setq unit (cons obj unit))
      (setq i (1+ i))
      (if (= i size)
	  (progn
	    (setq dest (cons (reverse unit) dest))
	    (setq unit nil)
	    (setq i 0)
	    ))
      (setq p (1+ p))
      )
    (if unit
	(setq dest (cons (reverse unit) dest))
      )
    (reverse dest)
    ))


;;; @ end
;;;

(provide 'mel-b)

;;; mel-b.el ends here.

