;; INITIAL CONFIGS

(require 'package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(ispell-dictionary nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(solaire-mode helpful company rainbow-mode beacon focus focus-autosave-mode markdown-preview-mode markdown-mode auto-theme-changer use-package solarized-theme haskell-mode))
 '(zoom-size '(0.618 . 0.618)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(package-initialize)

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'core)

(add-to-list 'load-path (expand-file-name "interface" user-emacs-directory))
(require 'interface)

(add-to-list 'load-path (expand-file-name "editing" user-emacs-directory))
(require 'editing)

(add-to-list 'load-path (expand-file-name "languages" user-emacs-directory))
(require 'languages)

(add-to-list 'load-path (expand-file-name "miscellaneous" user-emacs-directory))
(require 'miscellaneous)
