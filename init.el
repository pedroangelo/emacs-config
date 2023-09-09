;; INITIAL CONFIGS

(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(ispell-dictionary nil)
 '(package-archives
	 '(("gnu" . "https://elpa.gnu.org/packages/")
		 ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages '(haskell-mode solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(package-initialize)

;; CUSTOM SCRIPTS

;; set custom options, such as setting variables and enabling / disabling modes
(load "~/.emacs.d/scripts/set-custom-options")

;; automatic package installer
(load "~/.emacs.d/scripts/auto-package-installer")

;; automatic theme changer
;; (load "~/.emacs.d/auto-theme-changer")
