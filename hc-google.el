;;; hc-google.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; http://emacsredux.com/blog/2013/03/28/google/

;; ------------------------------------------------------------------------------
;; * Google Search

(defun hcGoogle ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url ;; results in default browser
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; ------------------------------------------------------------------------------
;; * Google Contacts

;; http://julien.danjou.info/projects/emacs-packages#google-contacts

;; uses oauth2 (in ELPA)

;; M-x google-contacts

;; key bindings:
;; n or p : go the next or previous record;
;; g      : refresh the result, bypassing the cache;
;; m      : send an e-mail to a contact;
;; s      : new search;
;; q      : quit.
(use-package google-contacts :defer t)

;; integrate directly Google Contacts into Gnus;
;; (use-package google-contacts-gnus :defer t)
;; Then use ; to go to contact info while reading an e-mail.

;; integrate directly Google Contacts into message-mode;
;; (use-package google-contacts-message :defer t)
;; Then use TAB to go to complete e-mail addresses in the header fields.

;; First time use:
;; - M-x google-contacts
;; - "Enter the code your browser displayed: "
;; - browser shows accounts.google.com/... saying
;;   google-oauth-el would like to manage your contacts
;;   CLICK: Accept
;; - Gives code.
;; - Cut/paste into above.
;; - "Passphrase for PLSTORE  plstore .../.emacs.d/oauth.plstore
;; - enter and store in password manager"

;; ------------------------------------------------------------------------------
;; * Google Maps

;; http://julien.danjou.info/projects/emacs-packages#google-maps

;; M-x google-maps
;; - type a location.
;;
;; key bindings:
;;
;; + or - to zoom in or out;
;; left, right, up, down to move;
;; z to set a zoom level via prefix;
;; q to quit;
;; m to add or remove markers;
;; c to center the map on a place;
;; C to remove centering;
;; t to change the maptype;
;; w to copy the URL of the map to the kill-ring;
;; h to show your home.

;; Integrate into Org-mode:

(use-package org-location-google-maps :defer t)

;; Then use C-c M-L to enter a location assisted by Google geocoding service.
;; Pressing C-c M-l will show you a map.

;; Advanced: look at google-maps-static-show and google-maps-geocode-request functions.

;; NOTE: home set via calendar-latitude/calendar-longitude

(use-package google-maps :defer t)

(provide 'hc-google)

;;; hc-google.el ends here
