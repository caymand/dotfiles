;; Melpa stuff
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(package-initialize)

;; Look
(set-face-attribute 'default nil :height 140 :font "Monospace")
(global-linum-mode 't)
(column-number-mode 't)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq tool-bar-style 'image)
(set-fill-column 81)
(require 'ido)
(set-face-attribute 'region nil :background "#666")

;; Make terminal emacs understand ctrl+shift key-chords
(require 'term-keys)
(term-keys-mode t)

;; Dired
(setq dired-lising-switches "-aBhl --sort=time")

;; Modes
; Just give me tabs in plain-text!!!
(add-hook 'text-mode-hook
          (lambda ()
            (define-key text-mode-map "\t" 'tab-to-tab-stop)))
; Org
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c c c") 'org-latex-export-to-pdf)))

; Latex
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

; Python
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Vagrant tramp

; Everything Lisp <3
(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/.emacs.d/lisp")
(use-package haskell-mode
  :ensure t
  )
; Haskell <>
(add-hook 'haskell-mode-hook (lambda ()
			       (local-set-key (kbd "C-c C-l") 'hs-lint)
			       (format-all-mode t)))
;; Other 
(ido-mode t)
(eval-after-load "flyspell" '(progn
  (define-key flyspell-mouse-map (kbd "<C-down-mouse-1>") #'flyspell-correct-word)
  (define-key flyspell-mouse-map (kbd "<C-mouse-1>") 'undefined) ))


;; Code formating
(use-package format-all
  :ensure t
  )	    
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(setq format-all-show-errors 'errors)

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
)

;; Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
   '(auctex vagrant-tramp term-keys slime ein vterm persp-mode flycheck which-key use-package format-all)))
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
