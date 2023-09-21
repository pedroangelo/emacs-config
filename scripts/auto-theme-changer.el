;; AUTO THEME CHANGER
;; automatically changes theme according to chosen times

; set variable with current theme
(setq current-theme nil)

; list of themes with corresponding starting times
(setq list-theme-times
			'(("05:00" . solarized-light)
				("20:00" . solarized-dark)
				("22:00" . solarized-dark-high-contrast)))

;; (nth 1 chosen-themes)
;; (car (nth 1 chosen-themes))
;; (cdr (nth 1 chosen-themes))

;; list of starting times
(setq themes-starting-time
	(mapcar 'car list-theme-times))

;; list of themes
(setq list-themes
	(mapcar 'cdr list-theme-times))

(defun set-theme-timer (time)
	"set a timer to call theme handler function"
	(run-at-time time 3600 'auto-theme-handler))

(defun set-all-theme-timers ()
	"set timers for each theme's starting time"
	(mapcar 'set-theme-timer themes-starting-time))

(defun current-hour ()
	"function to get the current hour"
	(string-to-number 
	 (substring (current-time-string) 11 13)))

(defun current-minute ()
	"function to get the current second"
	(string-to-number 
	 (substring (current-time-string) 14 16)))

;; (defun get-new-theme ()
;; 	"check which theme to choose according to current time of day"
;; 	(nil))

(defun get-time-intervals ()
	"from list-theme-times list, build new list with pairs of time intervals"
	(setq time-intervals 
				(cl-mapcar #'cons themes-starting-time (cdr themes-starting-time)))
	(setq final-time-interval
				(cons 
				 (nth (- (length themes-starting-time) 1) themes-starting-time)
				 (nth 0 themes-starting-time)))
	(append time-intervals (cons final-time-interval nil)))

(defun get-time-intervals-themes ()
	"from list-theme-times list, build new list with pairs of time intervals and corresponding themes"
	(cl-mapcar #'cons (get-time-intervals) list-themes))

	
;; (defun auto-theme-changer ()
;; 	"change theme automatically according to time of day"
;; 	(setq new-theme get-new-theme)
;; 	(if (equal new-theme current-theme)
;; 			nil
;; 		  (setq current-theme new-theme)
;; 			(load-theme current-theme t)))

;; ;; update theme on start
;; (auto-theme-changer)
