(use-package company
  :ensure t
  :config
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'editing.auto-completion) 
;;; editing.auto-completion.el ends here
