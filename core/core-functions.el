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

(provide 'core-functions)
;;; core-functions.el ends here
