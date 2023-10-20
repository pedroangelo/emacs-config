(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode))

(use-package dashboard
  :ensure t
  :init
  ;; Set the title
  (setq dashboard-banner-logo-title "Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  ;; Center content
  (setq dashboard-center-content t)
  ;; set dashboard items
  (setq dashboard-items '((bookmarks . 5)
                          (recents  . 5)))
  (use-package all-the-icons
    :if (display-graphic-p))
  ;; use all-the-icons package
  (setq dashboard-icon-type 'all-the-icons)
  ;; add icons to the widget headings and their items
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; show navigator below the banner
  (setq dashboard-set-navigator t)
  ;; show info about the packages loaded and the init time:
  (setq dashboard-set-init-info t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
            "Reload Configs" "Reload configurations from dot files"
            (lambda (&rest _) (load-file (expand-file-name "init.el" user-emacs-directory)))
            nil "" ""))))
  :config
  (dashboard-setup-startup-hook))

(provide 'features-editor) 
;;; features-editor.el ends here
