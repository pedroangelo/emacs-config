(use-package dimmer
  :config
  (dimmer-mode t)
  ;; set dimmer to only apply to foreground
  (setq dimmer-adjustment-mode :foreground)
  ;; set dimmer to dim 35%
  (setq dimmer-fraction 0.35))

(provide 'interface.focus-distractions)
;;; interface.focus-distractions.el ends here
