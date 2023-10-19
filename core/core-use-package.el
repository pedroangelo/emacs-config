(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
	(add-to-list 'load-path (expand-file-name "packages/whisper" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "packages/theme-rotation" user-emacs-directory))
  (require 'use-package))

;; install missing packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; update outdated packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide 'core-use-package) 
;;; core-use-package.el ends here
