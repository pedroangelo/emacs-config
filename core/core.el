(require 'core.configurations)
(require 'core.utilities)

(add-to-list 'load-path (expand-file-name "core/package-management" user-emacs-directory))
(require 'core.package-management)
(require 'core.libraries)

(provide 'core) 
;;; core.el ends here
