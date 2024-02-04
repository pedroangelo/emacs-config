(use-package company
  :pin gnu
  :config
  ;; (setq company-idle-delay nil)
  (setq company-dabbrev-downcase nil)  
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'editing.auto-completion) 
;;; editing.auto-completion.el ends here
