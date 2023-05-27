(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(toggle-debug-on-error 't)

;; Look
(setq font "Monaco-18")
(if (eq system-type 'darwin)
	(setq font "Monaco-18"))
(set-face-attribute 'default nil :weight 'Regular :font (eval 'font))
(global-linum-mode 't)
(global-visual-line-mode 't)
(setq-default fill-column 80)
(setq-default tab-width 4)
(column-number-mode 't)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq tool-bar-style 'image) ;; only useful when i actually have a tool bar
;;(load-theme 'afternoon 't)

;; Dired
(setq dired-lising-switches "-aBhl --sort=time")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Modes
(add-hook 'text-mode-hook (lambda ()
							(auto-fill-mode )
							(flyspell-mode )))
(add-hook 'text-mode-hook
          (lambda ()
            (define-key text-mode-map "\t" 'tab-to-tab-stop)
			(define-key text-mode-map "\C-c\C-a" 'artist-mode)))


(add-hook 'asm-mode-hook (lambda () 
						   (setq electric-indent-mode nil)))

(use-package elpy
  :ensure t
  :init (elpy-enable)
  :bind ("C-c C-r f" . elpy-black-fix-code))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'hs-lint)
(use-package haskell-mode
  :ensure t
  )
(add-hook 'haskell-mode-hook (lambda ()
			       (local-set-key (kbd "C-c C-l") 'hs-lint)
			       (format-all-mode t)))
(require 'ido)
(ido-mode t)
(eval-after-load "flyspell" '(progn
  (define-key flyspell-mouse-map (kbd "<C-down-mouse-1>") #'flyspell-correct-word)
  (define-key flyspell-mouse-map (kbd "<C-mouse-1>") 'undefined) ))

(with-eval-after-load "persp-mode"
 (setq wg-morph-on nil)
 (setq persp-autokill-buffer-on-remove 'kill-weak)
 (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)

(use-package ein
  :ensure t)

(use-package erlang
  :ensure t)

(grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e ''" . 27))

;; Terminal
(use-package vterm
  :ensure t)

;; Code formating
(use-package format-all
  :ensure t
  )	    
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(setq format-all-show-errors 'errors)
(set-fill-column 81)
(auto-fill-mode 't)

(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-black-fix-code nil t)))

;; Keys
(use-package which-key
  :ensure t
  :config (which-key-setup-side-window-bottom)
  )
(which-key-mode)

(defmacro global-set-keys (&rest keycommands)
  "Register keys to commands.
Analyze KEYCOMMANDS in pairs, and maps the corresponding keys
to the corresponding functions."
  (let ((setkey-list nil))
    (while keycommands
      (let ((key (car keycommands))
            (command (cadr keycommands)))
        (push `(global-set-key (kbd ,key)
                               ,command)
              setkey-list))
      (setq keycommands (cddr keycommands)))
    (push 'progn setkey-list)
    setkey-list))

(global-set-keys
 "\C-c b p" 'previous-buffer
 "\C-c b n" 'next-buffer
 "\C-c c c" 'compile
 "\C-c g" 'grep-find
 "\C-c b b" 'beginning-of-buffer
 "\C-c b e" 'end-of-buffer
)

(add-hook 'org-mode-hook
	  (lambda () (local-set-key (kbd "C-c c c") 'org-latex-export-to-pdf))
)

;; Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("919fabfc5cb6165ce07b9d8668f78fe75fe8bd08566006bc87513c29b4f34ade" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "49e12929187e8e8ef9ea5f816a21806a0a1966a4a8dd1d7eb4b8e911a187f0db" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" default))
 '(format-all-default-formatters
   '(("Assembly" asmfmt)
	 ("ATS" atsfmt)
	 ("Bazel" buildifier)
	 ("BibTeX" emacs-bibtex)
	 ("C" clang-format)
	 ("C#" clang-format)
	 ("C++" clang-format)
	 ("Cabal Config" cabal-fmt)
	 ("Clojure" zprint)
	 ("CMake" cmake-format)
	 ("Crystal" crystal)
	 ("CSS" prettier)
	 ("Cuda" clang-format)
	 ("D" dfmt)
	 ("Dart" dart-format)
	 ("Dhall" dhall)
	 ("Dockerfile" dockfmt)
	 ("Elixir" mix-format)
	 ("Elm" elm-format)
	 ("Emacs Lisp" emacs-lisp)
	 ("Erlang" efmt)
	 ("F#" fantomas)
	 ("Fish" fish-indent)
	 ("Fortran Free Form" fprettify)
	 ("GLSL" clang-format)
	 ("Go" gofmt)
	 ("GraphQL" prettier)
	 ("" nil)
	 ("Haskell" ormolu)
	 ("HTML" html-tidy)
	 ("HTML+ERB" erb-format)
	 ("Java" clang-format)
	 ("JavaScript" prettier)
	 ("JSON" prettier)
	 ("JSON5" prettier)
	 ("Jsonnet" jsonnetfmt)
	 ("JSX" prettier)
	 ("Kotlin" ktlint)
	 ("LaTeX" latexindent)
	 ("Less" prettier)
	 ("Literate Haskell" brittany)
	 ("Lua" lua-fmt)
	 ("Markdown" prettier)
	 ("Nix" nixpkgs-fmt)
	 ("Objective-C" clang-format)
	 ("OCaml" ocp-indent)
	 ("Perl" perltidy)
	 ("PHP" prettier)
	 ("Protocol Buffer" clang-format)
	 ("PureScript" purty)
	 ("Python" black)
	 ("R" styler)
	 ("Reason" bsrefmt)
	 ("ReScript" rescript)
	 ("Ruby" rufo)
	 ("Rust" rustfmt)
	 ("Scala" scalafmt)
	 ("SCSS" prettier)
	 ("Shell" shfmt)
	 ("Solidity" prettier)
	 ("SQL" sqlformat)
	 ("Svelte" prettier)
	 ("Swift" swiftformat)
	 ("Terraform" terraform-fmt)
	 ("TOML" prettier)
	 ("TSX" prettier)
	 ("TypeScript" prettier)
	 ("V" v-fmt)
	 ("Verilog" istyle-verilog)
	 ("Vue" prettier)
	 ("XML" html-tidy)
	 ("YAML" prettier)
	 ("Zig" zig)
	 ("_Angular" prettier)
	 ("_Flow" prettier)
	 ("_Gleam" gleam)
	 ("_Ledger" ledger-mode)
	 ("_Nginx" nginxfmt)
	 ("_Snakemake" snakefmt)))
 '(haskell-check-command "hlint")
 '(package-selected-packages
   '(vagrant-tramp vagrant acme-theme swift-mode elpy naysayer-theme tao-theme afternoon-theme monokai-theme nimbus-theme github-theme erlang ein vterm persp-mode flycheck which-key use-package format-all)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Spell checking
(global-auto-revert-mode)
(setq ispell-list-command "--list")
(setq ispell-program-name "aspell")


;; Hooks
(add-hook 'org-mode-hook (lambda ()
			   (auto-fill-mode t)
			   (flyspell-mode t)))
(put 'scroll-left 'disabled nil)
