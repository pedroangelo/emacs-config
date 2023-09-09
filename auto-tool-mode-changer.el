; set variable with current theme
(setq current-tool-bar-mode '(tool-bar-mode -1))

(defun auto-theme-changer ()
	(setq hour 
				(string-to-number 
				 (substring (current-time-string) 11 13)))
	(if (member hour (number-sequence 0 24))
			(setq new-tool-bar-mode '(tool-bar-mode 1))
		  (setq new-tool-bar-mode '(tool-bar-mode -1)))
	(if (equal new-tool-bar-mode current-tool-bar-mode)
			nil
		  (setq current-tool-bar-mode new-tool-bar-mode)
			(eval new-tool-bar-mode)))

(auto-theme-changer)
