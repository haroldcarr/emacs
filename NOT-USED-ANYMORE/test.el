(defun hc (fullName)
  (let* ((attributes (nth 5 (file-attributes fullName)))
	 (left (lsh (car attributes) 16))
	 (right (cadr attributes))
	 (lastMod (+ left right)))
    (decode-time attributes)))

(hc "d:/home/carr/ws/rip-new-release")
(48 43 3 18 5 1999 2 t 3600)
(hc "d:/home/carr/ws/rip.zip")
(22 51 17 11 5 1999 2 t 3600)

(file-attributes "d:/home/carr/.skij")

(nil 1 5 5 (14144 57630) (14120 38275) (14120 38226) 207 "-rw-rw-rw-" nil 0 -59280364)
(decode-time '(14144 57630))
(14 40 4 18 5 1999 2 t 3600)
(decode-time '(14120 38275))
(15 23 18 29 4 1999 4 t 3600)
(decode-time '(14120 38226))
(26 22 18 29 4 1999 4 t 3600)


(hc "d:/home/carr/.skij")

(current-time)
(decode-time (current-time))
(43 2 23 17 5 1999 1 t 3600)
(decode-time '(14144 . 57630))
(14 40 4 18 5 1999 2 t 3600)
(decode-time '(14144 57630))
(14 40 4 18 5 1999 2 t 3600)


;(load "nj")
;(clean-dot-p)

;;;;;

(load "hcChangeWords")
; for xemacs
(setq directory-sep-char ?\/)
(hcChangeWords '(("Hello" . "Ex"))
		 (expand-file-name "~/.sync/.rsync/orb/examples/static")
		 (expand-file-name "~/.sync/.rsync/orb/examples/static")
		 t)

;;;;

(load "hcChangeWords")
(hcChangeWords '(("c:vlcom" . "c:vl-load-com")
		   ("c:vlreac" . "c:vl-load-reactors"))
		 "r:/coreacad/lpp"
		 "r:/coreacad/lpp"
		 t)

;;;;;;;;;;;;;;;;

(load "c:/home/carr/rpt/98-03-name-changes.lsp")
(load "hcChangeWords")
(hcChangeWords *basis*
	       "c:/TEMP/develop/LPP/R14/SAMPLES/reac-tst/rtransl.lsp"
	       "c:/TEMP/develop/LPP/R14/SAMPLES/reac-tst/rtransl.lsp"
	       t)

(hcChangeWords *basis*
	       "c:/TEMP/develop/LPP/R14/SAMPLES/reac-tst/rutils.lsp"
	       "c:/TEMP/develop/LPP/R14/SAMPLES/reac-tst/rutils.lsp"
	       t)

;;;;;;;;;;;;;;;

(load "check-mss")

(check-mss "c:/home/carr/rpt/98-03-name-changes.lsp"
	   "c:/TEMP/install/Fishlips.mms"
	   "c:/temp/develop")
;;;;;;;;;;;;;

(load "hc-dir2html")

(hc-dir2html "c:/home/carr/vlisp")
(hc-dir2html "d:/t040/coreACAD/lpp")
(hc-dir2html "d:/t044/coreACAD/lpp")


(defun tt (&optional er) er)
tt
(tt)
nil
(tt 4)
4
