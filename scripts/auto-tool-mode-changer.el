;; ; set variable with current theme
;; (setq current-tool-bar-mode '(tool-bar-mode -1))

;; (defun auto-theme-changer ()
;; 	(setq hour 
;; 				(string-to-number 
;; 				 (substring (current-time-string) 11 13)))
;; 	(if (member hour (number-sequence 0 24))
;; 			(setq new-tool-bar-mode '(tool-bar-mode 1))
;; 		  (setq new-tool-bar-mode '(tool-bar-mode -1)))
;; 	(if (equal new-tool-bar-mode current-tool-bar-mode)
;; 			nil
;; 		  (setq current-tool-bar-mode new-tool-bar-mode)
;; 			(eval new-tool-bar-mode)))

;; (auto-theme-changer)


;; (defun auto-theme-changer ()
;; 	(setq hour 
;; 				(string-to-number 
;; 				 (substring (current-time-string) 18 19)))
;; 	(if (member hour (number-sequence 0 5))
;; 			(eval '(tool-bar-mode 1))
;; 		  (eval '(tool-bar-mode -1))))

;; (run-with-timer 0 3600 auto-theme-changer)
