;; Load and set initial setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp")
(load-file "~/.emacs.d/lib.el")


;; Look and basic stuff
(setq-default fill-column 80)
(setq-default tab-width 4)
;; (set-face-attribute 'region nil :background "#666")
(setq column-number-mode t)
(setq split-width-threshold 140)
(add-to-list 'default-frame-alist '(font . "Monaspace Neon-16"))
(global-visual-line-mode 't) ;; Nice for documents where 80char limit is useless

(auto-fill-mode t)
(column-number-mode 't)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(global-font-lock-mode t)
(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")
(global-hl-line-mode 1)
(set-face-background 'hl-line "Gray15")
(require 'ido)
(ido-mode t)

(ido-everywhere t)
(setq ido-decorations
	  '("\n>> " "" "\n " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; Customizations
(defun load-todo-dir () "Load directory of todo files"
	   (interactive)
	   (dired todo-dir))
(defun load-todo () "Load my todo list"
	   (interactive)
	   (find-file todo-file))

;; Dired
(setq dired-lising-switches "-aBhlpF --sort=time")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Spell checking
(global-auto-revert-mode)
(setq ispell-list-command "--list")
(setq ispell-program-name "aspell")
(eval-after-load "flyspell" '(progn
							   (define-key flyspell-mouse-map (kbd "<C-down-mouse-1>") #'flyspell-correct-word)
							   (define-key flyspell-mouse-map (kbd "<C-mouse-1>") 'undefined) ))


;;;;;;Modes;;;;;;
(load-file "~/.emacs.d/modes.el")
(load-file "~/.emacs.d/code.el")
;;;;;;;;;;;;;;;;;

;; Keys
(use-package which-key
  :ensure t
  :config (which-key-setup-side-window-bottom))
(which-key-mode)

(defmacro global-set-keys (&rest keycommands)
  "Register keys to commands.
Analyze KEYCOMMANDS in pairs, and maps the corresponding keys
to the corresponding functions."
  (let ((setkey-list nil))
    (while keycommands
      (let ((key (car keycommands))
            (command (cadr keycommands)))
		(print command)
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
 "\C-c g g" 'grep-find
 "\C-c b b" 'beginning-of-buffer
 "\C-c b e" 'end-of-buffer
 )

;; Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d2e44214a7dc0bd5b298413ed6c3ba9719f1d96794d9de3bdf7a9808902fd098" "919fabfc5cb6165ce07b9d8668f78fe75fe8bd08566006bc87513c29b4f34ade" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "49e12929187e8e8ef9ea5f816a21806a0a1966a4a8dd1d7eb4b8e911a187f0db" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" default))
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
 '(package-selected-packages
   '(eglot adoc-mode futhark-mode markdown-mode vagrant-tramp vagrant acme-theme swift-mode elpy naysayer-theme tao-theme afternoon-theme monokai-theme nimbus-theme github-theme erlang ein vterm persp-mode flycheck which-key use-package format-all)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
