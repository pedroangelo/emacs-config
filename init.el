(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))
(package-initialize)

;; disable electric-indent-mode
(electric-indent-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; line numbers
(global-display-line-numbers-mode t)
;; (setq display-line-numbers-type 'relative)

;; thin cursor
;; (setq-default cursor-type 'bar)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; display matching parenthesis
(show-paren-mode 1)
