;;; hc-music-volume.el --- music          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun hc-mac-volume-get ()
  "Return current Mac-OS audio volume."
  (string-to-number (shell-command-to-string (concat "hcMacVolumeGet"))))

(defun hc-mac-volume-change (amount)
  "Change Mac-OS audio volume by AMOUNT."
  (let ((new-vol (+ (hc-mac-volume-get) amount)))
    (if (zerop (shell-command (format "hcMacVolumeSet %s" new-vol)))
        (message "Volume is %s%%" (hc-mac-volume-get))
      (message "non zero exit status: hcMacVolumeSet"))))

(provide 'hc-music-volume)

;;; hc-music-volume.el ends here
