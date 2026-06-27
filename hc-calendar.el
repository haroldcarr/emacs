;;; hc-calendar.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html#Calendar_002fDiary
;; http://emacswiki.org/emacs/CalendarMode

(hcSection "Calendar and Diary")

(defvar view-diary-entries-initially)
(defvar number-of-diary-entries)
(defvar calendar-latitude)
(defvar calendar-longitude)
(defvar diary-list-include-blanks)
(defun hcCalendar () "."
  (setq diary-file (concat (hcFsync) "/.emacs.diary"))
  ;(setq calendar-week-start-day 1) ; monday
  (setq calendar-offset -1)
  (setq view-diary-entries-initially t)
  (setq number-of-diary-entries 2)
  ;; This causes a debug error in emacs 24
  ;(setq mark-diary-entries-in-calendar t)
  ;; This causes fancy not to be displayed
  ;;(setq view-calendar-holidays-initially t)
  ;;(setq holidays-in-diary-buffer nil)
  (setq calendar-latitude  40.785188)
  (setq calendar-longitude -111.863011)
  (with-eval-after-load 'holidays
    (setq holiday-bahai-holidays  nil)
    (setq holiday-hebrew-holidays nil)
    (setq holiday-jewish-holidays nil)
  )
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (setq diary-list-include-blanks t)
  ;; not working: (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
  ;; Make sure starting calendar comes after auto-resizing of frame.
  ;; (if (hcLucidP)
  ;;    (defun diary-remind (form number)
  ;;      (eval form)))
  ;; Do not automatically show calendar on startup
  ;(if (file-exists-p diary-file)
  ;    (calendar))
  ;; Customize appt.el
  ;;(use-package appt :defer t)
  (cond (nil
	 ;; This seems to cause emacs to crash on nt
	 ;; and does not exist in xemacs
	 (setq appt-message-warning-time 15) ;; minutes
	 (setq appt-display-duration 60) ;; seconds
	 (add-hook 'diary-hook 'appt-make-list)
	 (let ((diary-display-hook 'ignore))
	   (diary)))))

(with-eval-after-load 'holidays
  (setq holiday-bahai-holidays nil
        holiday-hebrew-holidays nil
        holiday-jewish-holidays nil
        calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
                holiday-christian-holidays
                holiday-islamic-holidays
                holiday-oriental-holidays
                holiday-solar-holidays)))

(use-package calendar :defer t :config (hcCalendar))

;; ** Calendar Framework

;; https://github.com/kiwanami/emacs-calfw

;; M-x cfw:open-diary-calendar
(use-package calfw-cal :defer t)

(provide 'hc-calendar)

;;; hc-calendar.el ends here
