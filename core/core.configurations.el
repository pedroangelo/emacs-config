;; VARIABLES

;; ;; display thin cursor
;; (setq-default cursor-type 'bar)

;; ;; display line numbers relative to current cursor line
;; (setq display-line-numbers-type 'relative)

;; use spaces for indentation instead of tabs
(setq-default indent-tabs-mode nil)

;; kill the whole line (newline including), if at collumn zero; kill up to newline if not at column zero
(setq kill-whole-line t)

;; default tab width set to 2 spaces
(setq-default tab-width 2)

;; wrap text at words (doesn't treat as having a newline at the end)
(setq-default word-wrap t)

;; MODES

;; ;; save previous emacs session
;; (desktop-save-mode 1)

;; disable electric-indent-mode
(electric-indent-mode -1)

;; update buffer when file changes on disk
(global-auto-revert-mode 1)

;; display line numbers
(global-display-line-numbers-mode t)

;; ;; wrap text at words (treats as having a newline at the end)
;; (global-visual-line-mode t)

;; do not display menu bar
(menu-bar-mode -1)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; display matching parenthesis
(show-paren-mode 1)

;; do not display tool bar
(tool-bar-mode -1)

;; KEY BINDINGS

;; unset key bind to suspend emacs
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(provide 'core.configurations)
;;; core.configurations.el ends here
