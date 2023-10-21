;; AUTO PACKAGE INSTALLER
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

(defun string-adjust-width (string)
  "Adjust string width to width by moving newlines"
  (let ((clean-string (string-reset-width string)))
    (if (< (length "asassas") (window-width))
        clean-string
      clean-string)))

;; concat
;; window-width
;; (substring "ola" 0 1)
;; (truncate-string-to-width "olla" 4)
;; (substring "0123456789" 1 2)

(setq my-list-quotes (mapcar 'string-adjust-width (split-string (file-to-string "~/MEGA/Hobbies e Interesses/Quotes") "\n")))

;(setq my-list-quotes '("The man who makes everything that leads to happiness depends upon himself, and not upon other men, has adopted the very best plan for living happily.\nThis is the man of moderation\n, the man of manly character and of wisdom. - Plato"))

(provide 'core-functions)
;;; core-functions.el ends here
