(provide 'modes)
;; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(setq haskell-process-type 'cabal-repl)

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)))

(custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
; TODO: Format on save


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











      
