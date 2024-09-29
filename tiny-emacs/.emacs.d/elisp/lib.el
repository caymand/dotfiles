(provide 'lib)

(defun load-init-file ()
  (interactive)
  (find-file-other-window user-init-file))
