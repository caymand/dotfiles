(provide 'modes)
;; Haskell
;; TBD

;; C/C++/CUDA

(if (eq system-type 'windows-nt)
    (setq build-file "build.bat")
  (setq build-file "build.sh"))

(defun compile-bat ()
  "Finds the build file and runs the build file in its directory.
When done, cd back to original directory"
  (interactive)
  (let ((build-file-dir (locate-dominating-file "." build-file)))
    (if build-file-dir
        (let ((default-directory build-file-dir))          
          (compile build-file)
          (other-window 1)
          ;; This makes go to build error/warning work
          (setq default-directory (concat build-file-dir "/build"))))))

;; hanging-braces-alist controls newlines
(c-add-style "my-cpp-style"
	     '("bsd"
	       (c-basic-offset . 4)   ; 4 space indentation
               (c-hanging-braces-alist
                (brace-list-open)
                (brace-list-close))
	       (c-offsets-alist
		(statement-case-intro . +)
		(case-label . +))		
	       ;; (c-offsets-alist
	       ;; 	(substatement-open . 0)
	       ;; 	(inline-open . 0)
	       ;; 	(block-open . 0)
	       ;; 	(defun-open . 0)
	       ;; 	(brace-list-open . 0))
	       (c-hanging-semi&comma-criteria no-newline-semicolon)))

(add-hook 'c-mode-common-hook (lambda ()
				(c-toggle-auto-newline 1)
				(c-set-style "my-cpp-style")))

(add-hook 'c++-mode-hook (lambda ()
			   (keymap-local-set "C-c C-c" 'compile-bat)))

(add-to-list 'auto-mode-alist '("\.cu$" . c++-mode))











      
