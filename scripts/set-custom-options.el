;; CONFIGURATION VARIABLES

; disable electric-indent-mode
(electric-indent-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; line numbers
(global-display-line-numbers-mode t)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; display matching parenthesis
(show-paren-mode 1)

;; update buffer when file changes on disk
(global-auto-revert-mode 1)

;; ENABLE / DISABLE MODES

;; default tab width
(setq-default tab-width 2)

;; wrap text at words (treats as not having a newline at the end)
(setq-default word-wrap t)

;; UNUSED

;; display line numbers relative to current cursor line
;; (setq display-line-numbers-type 'relative)

;; display thin cursor
;; (setq-default cursor-type 'bar)

;; wrap text at words (treats as having a newline at the end)
;; (global-visual-line-mode t)

;; save previous emacs session
;(desktop-save-mode 1)
