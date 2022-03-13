;;;;
;;;; Created       : 1999 Jan 30 (Sat) 11:51:02 by Harold Carr.
;;;; Last Modified : 1999 Dec 11 (Sat) 20:19:45 by Harold Carr.
;;;;

(defmacro define-global (name value doc)
  (let ((var-name (intern (concat "*" (symbol-name name) "*"))))
    `(progn
       (defvar ,var-name nil ,doc)
       (defun ,name (&optional arg)
	 (if arg
	     (setq ,var-name arg)
	     ,var-name))
       (if (null ,var-name) 
	   (,name ,value)))))

(define-global hcAddDat-addressBookPathAndFilename
  (expand-file-name "~/.sync/gnu/datlets/addressBook.dat")
  "The full absolute path to the address book datlet.")

(define-global hcAddDat-datletPathAndDirectory
  (expand-file-name "~/.sync/gnu/datlets")
  "The full absolute path to the datlet code.")

(define-global hcAddDat-clispPath
  (concat *hcUsrLocalDir* "/usr/local/lisp/clisp/clisp-1997-09-19")
  "The full absoluate path to the CLISP directory.")

(define-global hcAddDat-clispProgram
  (concat (hcAddDat-clispPath)
	  (char-to-string directory-sep-char)
	  "lisp")
  "The full path and name of the CLISP executable")

(define-global hcAddDat-clispArgs
  (concat (hcAddDat-clispPath)
	  (char-to-string directory-sep-char) 
	  "lispinit.mem")
  "Arguments to clisp program.")

(define-global hcAddDat-process-name "hcAddDat-process"
  "The emacs process name.")

(define-global hcAddDat-bufferName "*hcAddDat-process*"
  "The address book process output buffer.")
	  
;;;;
;;;; Low-level process control and communication.
;;;;

(defun hcAddDat-sendString (str)
  (process-send-string (hcAddDat-process) (concat str "\n")))

(defvar *hcAddDat-process* nil)

(defun hcAddDat-process ()
  (cond ((null *hcAddDat-process*)
	 (hcAddDat-startProcess)
	 (hcAddDat-waitForOutput)
	 (hcAddDat-loadDatAndAddressDat)
	 (hcAddDat-waitForOutput)))
  *hcAddDat-process*)

(defun hcAddDat-startProcess ()
  ;; ***** need to check process-status
  ;; CD necessary because of PAIP code.
  (cd (hcAddDat-datletPathAndDirectory))
  (setq *hcAddDat-process*
	(start-process (hcAddDat-process-name)
		       (hcAddDat-bufferName)
		       (hcAddDat-clispProgram)
		       "-M"
		       (hcAddDat-clispArgs)))
  (hcAddDat-filterExpecting 'discard)
  (set-process-filter *hcAddDat-process*
		      'hcAddDat-outputFilter)
  *hcAddDat-process*)

(defun hcAddDat-loadDatAndAddressDat ()
  (let ((datletDir (concat (hcAddDat-datletPathAndDirectory)
			   (char-to-string directory-sep-char))))
    (hcAddDat-sendString 
     (concat "(progn "
	     " (load \"" (concat datletDir "hc.cl") "\")"
	     " (lp)"
	     " (load \"" (concat datletDir "datlet.cl") "\")"
	     " (load \"" (hcAddDat-addressBookPathAndFilename) "\"))"))))

(defun hcAddDat-waitForOutput ()
  (accept-process-output (hcAddDat-process)))

(defun hcAddDat-kill ()
  (if (and (processp *hcAddDat-process*)
	   (member (process-status *hcAddDat-process*) '(run stop open)))
      (quit-process *hcAddDat-process*))
  (kill-buffer (hcAddDat-bufferName))
  (setq *hcAddDat-process* nil))

;;;;
;;;; Process output filters.
;;;;

(define-global hcAddDat-filterExpecting 'discard
  "Tells the filter what to do.")

(define-global hcAddDat-lastOutput ""
  "The last output received from the process.")

(defun hcAddDat-outputFilter (proc string)
  (hcAddDat-lastOutput string)
  ;; ***** make data-driven
  (cond ((eq 'discard (hcAddDat-filterExpecting))
	 ; ***** for development
	 (hcAddDat-putInBuffer proc string))
	((eq 'debug  (hcAddDat-filterExpecting))
	 (hcAddDat-putInBuffer proc string))
	((eq 'phoneNumber (hcAddDat-filterExpecting))
	 (hcAddDat-filterPhoneNumber proc string))
	(t
	 (print(concat "unknown: " (symbol-name (hcAddDat-filterExpecting))))
	 (hcAddDat-putInBuffer proc string))))

(defun hcAddDat-putInBuffer (proc string)
  ;; ***** replace with with-current-buffer in v20. see page 704
  (let ((cbuf (current-buffer)))
    (unwind-protect 
	(progn
	  (set-buffer (process-buffer proc))
	  (let ((moving (= (point) (process-mark proc))))
	    (save-excursion
	      (goto-char (process-mark proc))
	      (insert string)
	      (set-marker (process-mark proc) (point)))
	    (if moving (goto-char (process-mark proc)))))
      (set-buffer cbuf))))

(defun hcAddDat-filterPhoneNumber (proc string)
  ;; ***** development only
  (hcAddDat-putInBuffer proc string)
  nil)

;;;;
;;;; Process input abstraction.
;;;;

(defun getPhoneNumber (lastName)
  (hcAddDat-filterExpecting 'phoneNumber)
  (hcAddDat-sendString 
   (concat "(writeIndex (keyword (addressBook " lastName "))"
	   "            (name (?name))"
	   "            (voice (?voice)))"))
  (hcAddDat-waitForOutput)
  (cond ((noP)
	 (message (concat "Not found: " lastName)))
	(t
	 (let ((cbuf (current-buffer))
	       (response nil)
	       (no-seen nil))
	   (switch-to-buffer (hcAddDat-bufferName))
	   (while (not no-seen)
	     (setq response (read-from-minibuffer "More?: "))
	     (cond ((or (string-equal response ";")
			(string-equal response "."))
		    (hcAddDat-sendString response)
		    (hcAddDat-waitForOutput)
		    (setq no-seen (noP)))))
	   (read-from-minibuffer "Press any key to continue.")
	   (switch-to-buffer cbuf)))))

;;;;
;;;; Utilities
;;;;

(defun noP ()
  (stringMember "No." (hcAddDat-lastOutput)))

(defun stringMember (this that)
  (let* ((i 0)
	 (found nil)
	 (len-this (length this))
	 (len-that (length that))
	 (moreToDo (stringMemberMoreToDo i len-this len-that)))
    (while (and (not found) moreToDo)
      ;; ***** replace with v20 compare-strings
      (let ((ss (substring that i (+ len-this i))))
	(setq found (string-equal this ss))
	(setq i (+ i 1))
	(setq moreToDo (stringMemberMoreToDo i len-this len-that))))
    found))

(defun stringMemberMoreToDo (i len-this len-that)
  (<= i (- len-that len-this)))

;;;;
;;;; Address book commands.
;;;;

;;;;
;;;; Test
;;;;

;(load-file "hcAddDat.el")
;(hcAddDat-process)
; needs decoupling between start and use
;(getPhoneNumber "Ause")
;(getPhoneNumber "Anderson")
;(getPhoneNumber "Nonexistent")
;(hcAddDat-kill)
;
;(process-status *hcAddDat-process*)
;(kill-process *hcAddDat-process*)
;(setq *hcAddDat-process* nil)
;(hcAddDat-sendString ".")

;;; End of file.

