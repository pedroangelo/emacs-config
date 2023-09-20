;; AUTO DARK MODE
;; automatically changes theme according to time of day
;; interesting themes: solarized-dark-high-contrast, solarized-light, solarized-selenized-dark, solarized-selenized-white

; set variable with current theme
(setq current-theme nil)

;; function to get the current hour
(defun current-hour ()
	(string-to-number 
	 (substring (current-time-string) 11 13)))

;; function to get the current second
(defun current-second ()
	(string-to-number 
	 (substring (current-time-string) 18 19)))

;; change theme automatically according to time of day
(defun auto-dark-mode ()
	(setq hour (current-hour))
	(if (member hour (number-sequence 05 20))
			(setq new-theme 'solarized-light)
		  (setq new-theme 'solarized-dark))
	(if (equal new-theme current-theme)
			nil
		  (setq current-theme new-theme)
			(load-theme current-theme t)))

;; update theme on start
(auto-dark-mode)

;; update theme at specific times
(run-at-time "05:00" 3600 'auto-dark-mode)
(run-at-time "20:00" 3600 'auto-dark-mode)
