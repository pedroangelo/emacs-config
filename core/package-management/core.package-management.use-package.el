(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

;; automatically install packages not present already
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; automatically update outdated packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide 'core.package-management.use-package) 
;;; core.package-management.use-package.el ends here
