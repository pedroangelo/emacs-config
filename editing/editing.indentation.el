(use-package aggressive-indent
  :config
  ;; (add-to-list 'aggressive-indent-excluded-modes 'emacs-lisp-mode)
  (global-aggressive-indent-mode 1))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(provide 'editing.indentation) 
;;; editing.indentation.el ends here
