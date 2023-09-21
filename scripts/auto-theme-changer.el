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
;; (setq time-interval (cons (time-string-to-date "19:00") (cons (time-string-to-date "18:00") nil)))

(defun time-date-to-string (date)
	"convert date format (pair) to string"
	(concat (number-to-string (car date)) ":" (number-to-string (cdr date))))

(defun time-string-to-date (string)
	"convert string to date format (pair)"
	(cons (string-to-number (substring string 0 2)) (string-to-number (substring string 3 5))))

(defun get-current-hour ()
	"get the current hour as number as a number"
	(string-to-number 
	 (substring (current-time-string) 11 13)))

(defun get-current-minute ()
	"get the current minute as a number"
	(string-to-number 
	 (substring (current-time-string) 14 16)))

(defun get-current-time ()
	"get current time in format (hh . mm)"
	(cons (get-current-hour) (get-current-minute)))

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

(defun is-time-before-inclusive-p (time1 time2)
	"check if time1 happens before time2, including time1 equals time2"
	(if (or (< (car time1) (car time2)) 
					(and (= (car time1) (car time2)) 
							 (<= (cdr time1) (cdr time2)))) t nil))

(defun is-time-before-exclusive-p (time1 time2)
	"check if time1 happens before time2, excluding time1 equals time2"
	(if (or (< (car time1) (car time2)) 
					(and (= (car time1) (car time2)) 
							 (< (cdr time1) (cdr time2)))) t nil))

(defun current-time-interval-p (time-interval)
	"check if current time is within argument time interval"
	(setq current-time (get-current-time))
	(setq starting-time (car time-interval))
	(setq ending-time (car (cdr time-interval)))
	; check if midnight is within time interval or not
	(if (is-time-before-inclusive-p starting-time ending-time)
			; then do a simple comparison
			(and
			 ; starting time comes before current time
			 (is-time-before-inclusive-p starting-time current-time)
			 ; current time comes before ending time
			 (is-time-before-exclusive-p current-time ending-time))
		  ; else 
		  (or 
			 ; starting time comes before current time
			 (is-time-before-inclusive-p starting-time current-time)
			 ; current time comes before ending time
			 (is-time-before-exclusive-p current-time ending-time))))

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
