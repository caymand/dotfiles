(load-library "lib.el")

;; Use treesitter
(require 'treesit)
;; Now use treesitter major mode for these modes
(push '(python-mode . python-ts-mode) major-mode-remap-alist)
(push '(c-mode . c-ts-mode) major-mode-remap-alist)
(push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
;; Load the language objects from here
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	 (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "master" "src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
(setq treesit-font-lock-level 4)


;; LSP
;; I use eglot for LSP. This is a great article for how to setup the client:
;; https://joaotavora.github.io/eglot/
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(Python . "pyright")))

;; Elisp
;; Setup TODO and NOTE highlight for some modes
(mapc (lambda (mode)
		(font-lock-add-keywords mode
								'(("\\<\\(TODO\\)" 1 'font-lock-warning-face t)
								  ("\\<\\(NOTE\\)" 1 'font-lock-doc-markup-face t))))
	  '(python-mode
		emacs-lisp-mode
		c++-mode
	    LaTeX-mode))

;; C/C++
(defun no-newline-semicolon()
  (save-excursion
	'stop))

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
	;; (c-toggle-auto-newline 1)
								;; (c-toggle-syntactic-indentation 1))

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
  ;; (setq TeX-install-font-lock 'tex-font-setup)
  (setq TeX-install-font-lock 'font-latex-setup)
  ;; (setq TeX-newline-function #'newline-and-indent)
  (when (eq system-type 'darwin)
	(setq TeX-view-program-selection '((output-pdf "skim")))
	(setq TeX-view-program-list '(("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))


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

