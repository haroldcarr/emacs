;;; hc-time.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(use-package time
  :custom
  (world-clock-list
   '(("Pacific/Honolulu"     "Honolulu")
     ("America/Los_Angeles"  "San Francisco")
     ("America/Denver"       "Salt Lake City")
     ("America/Chicago"      "Austin")
     ("America/New_York"     "New York")
     ("America/Santiago"     "Santiago, Chile")
     ("Europe/London"        "London")
     ("Europe/Amsterdam"     "Amsterdam")
     ("Europe/Paris"         "Paris")
     ;;("Asia/Calcutta"        "Bangalore")
     ;;("Asia/Tokyo"           "Tokyo")
     ("Pacific/Auckland"     "Wellington, New Zealand")
     )))

(provide 'hc-time)

;;; hc-time.el ends here
