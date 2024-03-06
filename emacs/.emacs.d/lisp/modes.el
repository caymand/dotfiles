(load-library "lib.el") 

;; LSP
;; I use eglot for LSP. This is a great article for how to setup the client:
;; https://joaotavora.github.io/eglot/
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(Python . "pyright")))

;; Elisp
(mapc (lambda (mode)
		(font-lock-add-keywords mode
								'(("\\<\\(TODO\\)" 1 'font-lock-warning-face t))))
	  '(python-mode emacs-lisp-mode cc-mode))

;; C/C++
(defun c-mode-indentation ()
  (setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)
  (c-set-offset 'substatement-open '0))

(add-hook 'c-mode-common-hook (lambda ()
								(c-mode-indentation)))

;; Text
(add-hook 'text-mode-hook (lambda ()
							(auto-fill-mode)
							(flyspell-mode)
							(define-key text-mode-map "\C-c\C-a" 'artist-mode)))

;; Todo
(setq todo-show-with-done t)


;; Tex
(use-package tex
  :ensure auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-install-font-lock 'tex-font-setup)
  (setq TeX-newline-function #'newline-and-indent)
  ;; (setq TeX-indent-open-delimiters "{")
  ;; (setq TeX-indent-close-delimiters "}")
  ;; (setq TeX-install-font-lock 'font-latex-setup)
  (when (eq system-type 'darwin)
	(setq TeX-view-program-selection '((output-pdf "skim")))
	(setq TeX-view-program-list '(("skim" "open -a skim "))))
  (add-hook 'TeX-mode-hook (lambda ()
							 (auto-fill-mode -1))))
; This handy little things makes the docview buffer revert 
;; (add-hook
;;  'TeX-after-compilation-finished-functions
;;  #'TeX-revert-document-buffer)


;; ISPC
(use-package ispc-mode
  :ensure t)

;; Asm
(add-hook 'asm-mode-hook (lambda ()
						   (setq electric-indent-mode nil)))

;; Futhark
(add-hook 'futhark-mode-hook (lambda ()
							   (flycheck-mode)
							   (eglot-ensure)))
;; Ocaml

;; Markdown
(defun compile-md ()
  "Convert the markdown file to pdf"
  (interactive)
  (let* ((buf-parts (split-string (buffer-name) "\\."))
		 (stem (car buf-parts)))
	(shell-command (format "pandoc %s -o %s.pdf" (buffer-name) stem))))

(defun markdown-hooks ()
  (keymap-local-set "M-c" compile-md))

(add-hook 'markdown-mode-hook 'markdown-hooks)

;; Python
(defun get-python-function-name ()
  "Get the name of the current Python function."
  (interactive)
  (save-excursion
    (python-nav-beginning-of-defun)
    (re-search-forward "def[[:space:]]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil t)
    (match-string 1)))
(defun run-test ()
  (interactive "r")
  (let* ((test-name (get-python-function-name))
		 (cmd (format "pytest %s -k %s" buffer-file-name test-name)))
	(compile cmd)))
(add-hook 'python-mode-hook
		  (lambda ()
			(create-local-keys			 
			 "C-c c t" 'run-test)))
;; Alternatively we can add 'eglot-ensure to the python mode hook

;; Swift
(eval-after-load 'lsp-mode
  (progn
    (require 'lsp-sourcekit)
    (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
(add-hook 'swift-mode-hook (lambda () (lsp)))

