;;;;
;;;; Created       : 1999 Jun 04 (Fri) 11:19:04 by Harold Carr.
;;;; Last Modified : 2003 Sep 30 (Tue) 09:22:45 by Harold Carr.
;;;;

;;;
;;; TODO
;;; putback comments from file via: -m <filename>
;;;

(defvar *hcTwRipIntAnybodys* "")
(defun hcTwRipIntAnybodys () *hcTwRipIntAnybodys*)

(defvar *hcTwRipIntGenericAnybodys* "")
(defun hcTwRipIntGenericAnybodys () *hcTwRipIntGenericAnybodys*)

(defvar *hcTwRipIntPinyon* "")
(defun hcTwRipIntPinyon () *hcTwRipIntPinyon*)

(defvar *hcTwRipIntGenericPinyon* "")
(defun hcTwRipIntGenericPinyon () *hcTwRipIntGenericPinyon*)

(defvar *hcTwMinimumRipFiles* '())
(defun hcTwMinimumRipFiles () *hcTwMinimumRipFiles*)

(defvar *hcTwAllRipFilesButDeleted* '())
(defun hcTwAllRipFilesButDeleted () *hcTwAllRipFilesButDeleted*)

(defvar *hcTwBin*              nil)
(defvar *hcTwBringoverCommand* nil)
(defvar *hcTwPutbackCommand*   nil)
(defvar *hcTwMoveCommand*      nil)
(defvar *hcTwDeleteCommand*    nil)
(defvar *hcTwReparentCommand*  nil)


(defun hcTwSetVariables ()
  (setq *hcTwBin* (cond ((hcWin32P) "c:/SunWorkshop/TeamWarePC/bin")
			(t 
			 "/java/devtools/sparc/SUNWspro/SC6.1/bin"
			 ;; This has a bug with Merlin bringovers
			 ;;"/usr/dist/pkgs/devpro,v4.2/5bin.sun4/bin"
			 )))
  (setq *hcTwBringoverCommand*  (concat *hcTwBin* "/bringover"))
  (setq *hcTwPutbackCommand*    (concat *hcTwBin* "/putback"))
  (setq *hcTwMoveCommand*       (concat *hcTwBin* "/workspace move"))
  (setq *hcTwDeleteCommand*     (concat *hcTwBin* "/workspace delete"))
                                                             ;; delete -f -d
  (setq *hcTwReparentCommand*   (concat *hcTwBin* "/workspace parent -p"))

  (setq *hcTwRipIntAnybodys*        "/net/anybodys/export1/ws/rip-int")
  (setq *hcTwRipIntGenericAnybodys* "/net/anybodys/export3/ws/rip-int-generic")

  (setq *hcTwRipIntPinyon*   	    "/home/hcarr/ws/rip-int")
  (setq *hcTwRipIntGenericPinyon*   "/home/hcarr/ws/rip-int-generic")

  (setq *hcTwMinimumRipFiles*
	'( " ./Lib "
	   " ./make "
	   " ./src "
	   " ./test "
	   " ./test12 "
	   " ./values "
	   " ./values2 "
	   ))

  (setq *hcTwAllRipFilesButDeleted*
	'( " ./Lib "
	   " ./dbgsrc "
	   " ./docs "
	   " ./installer "
	   " ./make "
	   " ./notes "
	   " ./readme "
	   " ./readme.html "
	   " ./samples "
	   " ./src "
	   " ./sunperf "
	   " ./suntests11.ksh "
	   " ./suntests12.ksh "
	   " ./test "
	   " ./test12 "
	   " ./values "
	   " ./values2 "
	   ))
  )

(hcTwSetVariables)

;;;;
;;;; move
;;;;

(defun hcTwMove (from to)
  (concat *hcTwMoveCommand* " " from " " to))

;;;;
;;;; delete
;;;;

(defun hcTwDelete (ws)
  (concat *hcTwDeleteCommand* " " ws))

;;;;
;;;; reparent
;;;;

(defun hcTwReparent (newParent child)
  (concat *hcTwReparentCommand* " " newParent " " child))

;;;;
;;;; bringover/putback
;;;;

(defun hcTwBringoverOrPutback (preview-p comment from to &optional files)
  (let ((command (if comment *hcTwPutbackCommand* *hcTwBringoverCommand*)))
    (setq preview-p (if preview-p " -n " ""))
    (setq comment (if comment (concat " -c '" comment "' ") ""))
    (setq files (if (null files)
		    "./"
		  (apply #'concat
			 (mapcar #'(lambda (file) (concat file " "))
				 files))))
    (concat command " -w " to " -p " from comment preview-p " " files)))

(defun hcTwBringover (preview-p from to &optional files)
  (hcTwBringoverOrPutback preview-p nil     from to files))

(defun hcTwPutback (preview-p comment from to &optional files)
  (hcTwBringoverOrPutback preview-p comment from to files))

(defun hcTwBoPbRip (preview-p comment)
  (hcTwBringoverOrPutback preview-p comment (hcTwRipIntAnybodys) (hcTwRipIntPinyon)))

(defun hcTwBoRip (preview-p)
  (hcTwBoPbRip preview-p nil))

(defun hcTwPbRip (preview-p comment)
  (hcTwBoPbRip preview-p comment))

;;;;
;;;; Usage and test.
;;;;

(comment
(load-file "hcTw.el")

(hcTwMove (hcTwRipIntPinyon) 
	  (expand-file-name "~/ws/giop12"))

(hcTwDelete (hcTwRipIntPinyon))

(hcTwDelete (expand-file-name "~/ws/docs-1.3"))

(hcTwReparent (hcTwRipIntGenericAnybodys)
	      (hcTwRipIntPinyon))

;;; RIP-INT

(hcTwBringover t
	       (hcTwRipIntAnybodys)
	       (hcTwRipIntPinyon) 
	       (hcTwMinimumRipFiles)
	       )
;	       (hcTwAllRipFilesButDeleted)

(hcTwPutback t 
	     "Removed ServerLocation - reviewed by Ken."
	     (hcTwRipIntAnybodys) 
	     (hcTwRipIntPinyon)
	     (hcTwMinimumRipFiles)
	     )
;            (hcTwAllRipFilesButDeleted)

;'("./src/share/classes/com/sun/corba/se/internal/corba/ORB.java")


;;; RIP-INT-GENERIC

(hcTwBringover t 
	       (hcTwRipIntAnybodys) 
	       (hcTwRipIntGenericAnybodys)
	       (hcTwMinimumRipFiles))

(hcTwBringover t 
	       (hcTwRipIntGenericAnybodys)
	       (hcTwRipIntGenericPinyon)
	       (hcTwMinimumRipFiles))

(hcTwPutback t
	     "..."
	     (hcTwRipIntGenericAnybodys)
	     (hcTwRipIntGenericPinyon)
	     (hcTwMinimumRipFiles))

)

(provide 'hcTw)

;;; End of file.
