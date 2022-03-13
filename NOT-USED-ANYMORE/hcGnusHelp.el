(defun tt ()
  (interactive)
  (let* ((newsdir          "/home/carr/News")
	 (output-filename  (concat newsdir "/1"))
	 (process-name     (concat newsdir "/tt"))
	 (tt-output-buffer (get-buffer-create "tt-output")))
    (cd newsdir)
    (eserve-write-buffer "*Article*" output-filename)
    (call-process process-name		; name of process
		  nil			; stdin is /dev/null
		  tt-output-buffer	; stdout is this buffer
		  nil
		  output-filename)	; args to process
    (let* ((image-filename (buffer-string 1
					  (buffer-size tt-output-buffer)
					  tt-output-buffer))
	   (delete? (y-or-n-p (concat "Delete " image-filename " ? "))))
      ;; Kill now in case delete errors.
      (kill-buffer tt-output-buffer)
      (if delete?
	  (delete-file (concat newsdir "/" image-filename))))))

(gnus-define-keys gnus-summary-mode-map
  "v" tt)

;; End of file.



