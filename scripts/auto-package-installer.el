;; AUTO PACKAGE INSTALLER
;; automatically install packages from package-list not yet installed

;; set list of packages to install
(setq package-list
      '(haskell-mode
				solarized-theme))

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
