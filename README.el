;; save backup files in a specific folder, to avoid cluttering
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; display both current line and collumn numbers for pointer
(setq column-number-mode t)
(setq line-number-mode t)

;; ;; display thin cursor
;; (setq-default cursor-type 'bar)

;; ;; display line numbers relative to current cursor line
;; (setq display-line-numbers-type 'relative)

;; use spaces for indentation instead of tabs
(setq-default indent-tabs-mode nil)

;; ;; Do not show the startup screen.
;; (setq inhibit-startup-message t)

;; kill the whole line, including newline, if at collumn zero
;; kill up to newline if not at collumn zero
(setq kill-whole-line t)

;; default tab width set to 2 spaces
(setq-default tab-width 2)

;; wrap text at words (doesn't treat as having a newline at the end)
(setq-default word-wrap t)

;; place custom-set-variables and custom-set-faces into their own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
(load custom-file))

;; ;; save previous emacs session
;; (desktop-save-mode 1)

;; disable electric-indent-mode
(electric-indent-mode -1)

;; update buffer when file changes on disk
(global-auto-revert-mode 1)

;; display line numbers
(global-display-line-numbers-mode t)

;; highlight current line
(global-hl-line-mode t)

;; ;; wrap text at words (treats as having a newline at the end)
;; (global-visual-line-mode t)

;; do not display menu bar, tool bar and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; display matching parenthesis
(show-paren-mode 1)

;; unset key bind to suspend emacs
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(defun file-to-string (file)
  "Read file contents and return as string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun string-reset-width (string)
  "Remove all newline characters from string, effectively adjusting the width to size of string"
  (replace-regexp-in-string "\n" "" string))

(defun string-nth (n string)
  "Obtain char at the nth position in the string"
  (substring string n (+ n 1)))

;; (defun split-string-max-length (string max-length)
;;   "Split a string into substrings with a max length of max-length"
;;   (let ((new-string nil)
;;         (rest-string string))
;;     (let ((number-substrings (if (= 0 (mod (length string) max-length))
;;                                  (/ (length string) max-length) 
;;                                (+ 1 (/ (length string) max-length)))))
;;       (progn
;;         (dotimes (number number-substrings)
;;           (progn
;;             (setq new-string (concat new-string (seq-take rest-string max-length) "\n"))
;;             (setq rest-string (seq-drop rest-string max-length))))
;;         new-string))))

(defun string-adjust-width (string width)
  "Adjust string width to width by moving newlines"
  (let* ((clean-string (replace-regexp-in-string "\n" "" string))
         (length-string (length clean-string))
         (number-lines (ceiling (/ (float length-string) width)))
         (partition-size (ceiling (/ (float length-string) number-lines))))
    (if (< length-string width)
        clean-string
      (string-join (seq-partition clean-string partition-size) "\n"))))

(setq personal-quote-list (split-string (file-to-string "~/MEGA/Hobbies e Interesses/Quotes") "\n"))

(setq personal-quote-list-formatted (mapcar (lambda (quote) (string-adjust-width quote 
                                                                                 (truncate (* (window-width) 0.95))))
                                            personal-quote-list))

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

;; automatically install packages from package-list not yet installed

;; set list of packages to install
(setq package-list
      '(
				;haskell-mode
				;solarized-theme
				use-package))

;; function to test for internet connection
(defun test-internet-connection (&optional host)
	"Tests for internet connection by calling ping, sending one packet to either argument or 8.8.8.8."
	;; call-process program &optional infile destination display &rest args
	;; ping args: -c 1, stop after sending 1 packet; -W 1, timeout after 1 second
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
										 (if host host "8.8.8.8"))))

;; set variable has-internet-connection as: t (if there is internet connection) or nil (if there is not)
(setq has-internet-connection (if (test-internet-connection) t nil))

;; update package list
(defun update-package-list ()
	"If package list is not updated, and there is internet connection, update package list. Otherwise, do nothing."
	(if (and (not package-archive-contents) has-internet-connection)
			(ignore-errors (package-refresh-contents))
		  nil)
)

;; install missing packages
(defun install-missing-packages ()
	"If there is internet connection, install all uninstalled packages from package-list"
	(if has-internet-connection
			(dolist (package package-list)
				(unless (package-installed-p package)
					(ignore-errors (package-install package))))
		  nil)
)

; update package list
(update-package-list)

; install missing packages
(install-missing-packages)

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

(use-package dash
  :pin gnu)

(use-package seq
  :pin gnu)

(use-package s)

(use-package dashboard
  :init
  ;; set the title
  (setq dashboard-banner-logo-title "Emacs Dashboard")
  ;; set the banner
  (setq dashboard-startup-banner 'logo)
  ;; center content
  (setq dashboard-center-content t)
  ;; set dashboard items
  (setq dashboard-items '((bookmarks . 5)
                          (recents  . 5)))
  (use-package all-the-icons 
    :if (display-graphic-p))
  ;; use all-the-icons package
  ;; don't forget to M-x all-the-icons-install-fonts
  (setq dashboard-icon-type 'all-the-icons)
  ;; add icons to the widget headings and their items
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; show navigator below the banner
  (setq dashboard-set-navigator t)
  ;; show info about the packages loaded and the init time:
  (setq dashboard-set-init-info t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
            "Reload Configs" "Reload configurations from dot files"
            (lambda (&rest _) (load-file (expand-file-name "init.el" user-emacs-directory)))
            nil "" ""))))
  (setq dashboard-footer-messages personal-quote-list-formatted)
  (setq dashboard-footer-icon (all-the-icons-faicon "quote-left"
                                                    :height 1.1
                                                    :v-adjust -0.05
                                                    :face 'font-lock-keyword-face))
  :config
  (dashboard-setup-startup-hook))

(use-package zoom
  :config
  (zoom-mode t)
  ;; resize windows according to the golden ratio
  (custom-set-variables '(zoom-size '(0.618 . 0.618))))

(use-package solarized-theme)

(use-package circadian
  :config
  (setq calendar-latitude 41.1)
  (setq calendar-longitude -8.7)
  (setq circadian-themes '((:sunrise . solarized-light)
                           ("5:00" . solarized-light)
                           (:sunset . solarized-dark)
                           ("18:30" . solarized-dark)))
  (circadian-setup))

(use-package dimmer
  :config
  (dimmer-mode t)
  ;; set dimmer to only apply to foreground
  (setq dimmer-adjustment-mode :foreground)
  ;; set dimmer to dim 35%
  (setq dimmer-fraction 0.35))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode))

(use-package aggressive-indent
  :config
  ;; (add-to-list 'aggressive-indent-excluded-modes 'emacs-lisp-mode)
  (global-aggressive-indent-mode 1))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package company
  :pin gnu
  :config
  ;; (setq company-idle-delay nil)
  (setq company-dabbrev-downcase nil)  
  (add-hook 'after-init-hook 'global-company-mode))

(use-package origami
  :requires (dash s)
  :config
  (global-origami-mode))

(use-package haskell-mode)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("TODO\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; show whitespaces as dots
;; (add-hook 'latex-mode-hook 'whitespace-mode)

;; prevent truncating lines in org mode; similar to word-wrap
(setq org-startup-truncated nil)

;; open files with unfolded headings
(setq org-startup-folded nil)

(add-to-list 'load-path (expand-file-name "packages/whisper" user-emacs-directory))

(use-package whisper
  :load-path "~/.emacs.d/packages/whisper"
  :bind ("C-H-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/.local/lib"
				whisper-language "en"
				whisper-model "base" ;; model options: tiny, base, small, medium, large
        whisper-translate nil
				whisper-enable-speed-up nil))
