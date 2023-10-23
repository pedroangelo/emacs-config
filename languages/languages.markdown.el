(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("TODO\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(provide 'languages.markdown) 
;;; languages.markdown.el ends here
