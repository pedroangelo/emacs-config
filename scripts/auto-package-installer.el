;; AUTO PACKAGE INSTALLER
;; automatically install packages from package-list, if they are not installed

;; list packages
(setq package-list
      '(haskell-mode
				solarized-theme))

;; get list of available packages
(unless package-archive-contents
  (ignore-errors (package-refresh-contents)))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (ignore-errors (package-install package))))