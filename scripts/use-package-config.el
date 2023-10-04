(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
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

;; LOAD PACKAGES

(use-package haskell-mode)

(use-package solarized-theme)

(use-package auto-theme-changer
	:load-path "~/.emacs.d/scripts/auto-theme-changer.el"
	:init
	(setq user-theme-rotation
				'(("05:00" . solarized-light)
					("18:30" . solarized-dark)
					("22:30" . solarized-dark-high-contrast))))
