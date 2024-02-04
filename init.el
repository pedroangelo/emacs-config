;; INITIAL CONFIGS

(setq custom-file (concat user-emacs-directory "custom.el"))

(require 'package)

(package-initialize)

(org-babel-load-file "~/.emacs.d/README.org")
