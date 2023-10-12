;; ENABLE / DISABLE MODES

;; disable electric-indent-mode
(electric-indent-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; display line numbers
(global-display-line-numbers-mode t)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; display matching parenthesis
(show-paren-mode 1)

;; update buffer when file changes on disk
(global-auto-revert-mode 1)

;; CONFIGURATION VARIABLES

;; default tab width
(setq-default tab-width 2)

;; set indentation to spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; wrap text at words (doesn't treat as having a newline at the end)
(setq-default word-wrap t)

;; kills up to newline if not at column zero, kills the whole line (newline and all) if at column zero
(setq kill-whole-line t)

;; UNUSED

;; ;; display line numbers relative to current cursor line
;; (setq display-line-numbers-type 'relative)

;; ;; display thin cursor
;; (setq-default cursor-type 'bar)

;; ;; wrap text at words (treats as having a newline at the end)
;; (global-visual-line-mode t)

;; ;; save previous emacs session
;; (desktop-save-mode 1)
