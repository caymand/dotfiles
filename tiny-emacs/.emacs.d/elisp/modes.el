(provide 'modes)
;; Haskell
;; TBD
;; C/C++/CUDA

(c-add-style "my-cpp-style"
	     '("bsd"
	       (c-basic-offset . 4)
	       (c-offsets-alist
		(substatement-open . 0)
		(inline-open . 0)
		(block-open . 0)
		(defun-open . 0)
		(brace-list-open . 0))
	       (c-hanging-semi&comma-criteria no-newline-semicolon)))

(add-hook 'c-mode-common-hook (lambda ()
				(c-toggle-auto-newline 1)
				(c-set-style "my-cpp-style")))

(add-to-list 'auto-mode-alist '("\.cu$" . c++-mode))











      
