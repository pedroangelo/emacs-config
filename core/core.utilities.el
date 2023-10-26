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

(provide 'core.utilities)
;;; core.utilities.el ends here
