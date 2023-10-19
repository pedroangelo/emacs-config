(use-package whisper
  :load-path "~/.emacs.d/packages/whisper"
  :bind ("C-H-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/.local/lib"
				whisper-language "en"
				whisper-model "base" ;; model options: tiny, base, small, medium, large
        whisper-translate nil
				whisper-enable-speed-up nil))

(provide 'miscellaneous-whisper) 
;;; miscellaneous-whisper.el ends here
