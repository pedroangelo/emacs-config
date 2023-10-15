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

;; LOAD PACKAGES

(use-package haskell-mode)

(use-package solarized-theme)

(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode))

(use-package theme-rotation
	:load-path "~/.emacs.d/packages/theme-rotation"
	:custom
  (theme-rotation-config 
   '(("05:00" . solarized-light)
		 ("18:30" . solarized-dark)
		 ("22:30" . solarized-dark-high-contrast))
	 "customize theme-rotation-config to apply dark-mode after 18:30.")
  :config
  (theme-rotation-mode))

(use-package whisper
  :load-path "~/.emacs.d/packages/whisper"
  :bind ("C-H-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/.local/lib"
				whisper-language "en"
				whisper-model "base" ;; model options: tiny, base, small, medium, large
        whisper-translate nil
				whisper-enable-speed-up nil))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
