;; AUTO THEME CHANGER
;; automatically changes theme according to chosen times

; configuration of starting times for specific themes
(setq config-times-themes
			'(("05:00" . solarized-light)
				("18:00" . solarized-dark)
				("22:00" . solarized-dark-high-contrast)))

;; AUXILIARY FUNCTIONS AND VARIABLES

; set variable with current theme
(setq current-theme nil)

;; (nth 1 chosen-themes)
;; (car (nth 1 chosen-themes))
;; (cdr (nth 1 chosen-themes))
;; (concat (number-to-string (get-current-hour)) ":" (number-to-string (get-current-minute)))

(defun time-date-to-string (date)
	"convert date format (pair) to string"
	(concat (number-to-string (car date)) ":" (number-to-string (cdr date))))

(defun time-string-to-date (string)
	"convert string to date format (pair)"
	(cons (string-to-number (substring string 0 2)) (string-to-number (substring string 3 5))))

(defun get-current-hour ()
	"get the current hour as a string"
	(string-to-number 
	 (substring (current-time-string) 11 13)))

(defun get-current-minute ()
	"get the current minute as a string"
	(string-to-number 
	 (substring (current-time-string) 14 16)))

(defun get-list-starting-times ()
	"return a list of starting times"
	(mapcar 'car config-times-themes))

(defun get-list-themes ()
	"list of themes"
	(mapcar 'cdr config-times-themes))

(defun get-list-time-intervals ()
	"from config-times-themes list, build list with pairs of time intervals"
	(setq time-intervals
				(cl-mapcar #'cons (get-list-starting-times) (cdr (get-list-starting-times))))
	(setq final-time-interval
				(cons 
				 (nth (- (length (get-list-starting-times)) 1) (get-list-starting-times))
				 (nth 0 (get-list-starting-times))))
	(append time-intervals (cons final-time-interval nil)))

(defun get-list-time-intervals-themes ()
	"from config-times-themes list, build list with pairs of time intervals and corresponding themes"
	(cl-mapcar #'cons (get-list-time-intervals) (get-list-themes)))

;; MAIN FUNCTIONALITY

;; (defun get-new-theme ()
;; 	"check which theme to choose according to current time of day"
;; 	(nil))

(defun is-time-before-p (hour1 minute1 hour2 minute2)
	"check if hour1:minute1 happens before hour2:minute2 "
	(if (or (< hour1 hour2) (and (= hour1 hour2) (<= minute1 minute2))) t nil))

(defun is-time-after-p (hour1 minute1 hour2 minute2)
	"check if hour1:minute1 happens after hour2:minute2 "
	(if (or (> hour1 hour2) (and (= hour1 hour2) (> minute1 minute2))) t nil))

(defun current-time-interval-p (time-interval)
	"check if current time is within times of argument"
	(setq current-hour (get-current-hour))
	(setq current-minute (get-current-minute))
	(setq starting-hour (string-to-number (substring (car time-interval) 0 2)))
	(setq starting-minute (string-to-number (substring (car time-interval) 3 5)))
	(setq ending-hour (string-to-number (substring (cdr time-interval) 0 2)))
	(setq ending-minute (string-to-number (substring (cdr time-interval) 3 5)))
	; check if midnight is within time interval or not
	(if (is-time-before-p starting-hour starting-minute ending-hour ending-minute)
			(and
			 (is-time-before-p starting-hour starting-minute current-hour current-minute)
			 (is-time-after-p ending-hour ending-minute current-hour current-minute))
		  (or 
			 (is-time-before-p starting-hour starting-minute current-hour current-minute)
			 (is-time-after-p ending-hour ending-minute current-hour current-minute))))

;; (defun auto-theme-changer ()
;; 	"change theme automatically according to time of day"
;; 	(setq new-theme get-new-theme)
;; 	(if (equal new-theme current-theme)
;; 			nil
;; 		  (setq current-theme new-theme)
;; 			(load-theme current-theme t)))

;; TIMERS

(defun set-theme-timer (time)
	"set a timer to call theme changer function"
	(run-at-time time 3600 'auto-theme-changer))

(defun set-all-theme-timers ()
	"set timers for each theme's starting time"
	(mapcar 'set-theme-timer themes-starting-time))

;; START AUTO-THEME-CHANGER AND TIMERS

;; ;; update theme on start
;; (auto-theme-changer)
