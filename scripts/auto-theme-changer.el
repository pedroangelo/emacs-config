;; AUTO THEME CHANGER
;; automatically changes theme according to chosen times

; set variable with current theme
(setq current-theme nil)

; list of themes with corresponding starting times
(setq list-theme-times
			'(("05:00" . solarized-light)
				("20:00" . solarized-dark)))

;; (nth 1 chosen-themes)
;; (car (nth 1 chosen-themes))
;; (cdr (nth 1 chosen-themes))

;; list of starting times
(setq themes-starting-time
	(mapcar 'car list-theme-times))

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

(defun get-new-theme ()
	"check which theme to choose according to current time of day"
	nil)

(defun auto-theme-changer ()
	"change theme automatically according to time of day"
	(setq new-theme get-new-theme)
	(if (equal new-theme current-theme)
			nil
		  (setq current-theme new-theme)
			(load-theme current-theme t)))

;; update theme on start
(auto-theme-changer)
