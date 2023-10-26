(use-package solarized-theme)

(use-package circadian
  :config
  (setq calendar-latitude 41.1)
  (setq calendar-longitude -8.7)
  (setq circadian-themes '((:sunrise . solarized-light)
                           ("5:00" . solarized-light)
                           (:sunset . solarized-dark)
                           ("18:30" . solarized-dark)))
  (circadian-setup))

(provide 'interface.themes) 
;;; interface.themes.el ends here
