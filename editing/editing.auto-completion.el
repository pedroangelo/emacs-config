(use-package company
  :config
  (setq company-idle-delay nil)
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  :pin gnu)

(provide 'editing.auto-completion) 
;;; editing.auto-completion.el ends here
