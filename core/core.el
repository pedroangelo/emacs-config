(require 'core.configurations)
(require 'core.utilities)

(add-to-list 'load-path (expand-file-name "core/package-management" user-emacs-directory))
(require 'core.package-management)

(provide 'core) 
;;; core.el ends here
