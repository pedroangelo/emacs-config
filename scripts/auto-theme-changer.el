;; AUTO THEME CHANGER
;; automatically changes theme according to time of day


;; set variable with current theme
(setq current-theme ('solarized-light))

(defun auto-theme-changer ()
	(setq hour 
				(string-to-number 
				 (substring (current-time-string) 11 13)))
	(if (member hour (number-sequence 8 20))
			(setq chosen-theme ('
