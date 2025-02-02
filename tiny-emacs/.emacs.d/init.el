(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(global-company-mode t)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'modes)
(require 'lib)

;; Visual
;; (my-theme)
(add-hook 'after-init-hook (lambda () (load-theme 'my-theme t)))
(setq-default indent-tabs-mode nil)
(setq grep-command "rg --no-heading -nH ")
(setq grep-find-command "rg --no-heading --hidden ")
(setq column-number-mode t)
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Iosevka" :height 160)
  (set-face-attribute 'default nil :font "Iosevka" :height 140))
(vertico-mode)
(setq split-width-threshold 160)
;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Absolutelt essential for docview
(setq doc-view-mupdf-use-svg (image-type-available-p 'svg))

;; KEYS
(keymap-global-set "C-c c c" 'compile)

;; Dired
(setq dired-dwim-target t)

; Modes
(which-key-mode)
(global-auto-revert-mode)

;; Environment 
(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:/Program Files/Git/usr/bin;" (getenv "PATH"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '((".*" . "c:/Users/kak/AppData/Local/Temp")))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("6966e2016d70377c234a3f1d7abc3d2d213ca69d4b781dcfab8b4db2d3410111" "0c00bb5cd9f50d1887ec57cc6469ad031ce67b4f028b03cd9f02e39aaa15c14e" "651a94e42ab0bc2a9c1b7ec6368dd13e611584479840956413e1089458a224f8" "1e10b08a48f816adcab5c60cc7bd5a6272d7bf75e40a6fbc19d3baa20b1bd5ad" "f1c8202c772d1de83eda4765fe21429a528a4fb350a28394d3705fe9678ed1f9" "408cd3f51767ac82908b5c987ca8bd2ec76fb67c3478ee9e2b60fc6e6066e635" default))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   '(auctex gnuplot cuda-mode acme-theme go-mode orderless magit dumb-jump ormolu vertico which-key markdown-mode haskell-mode futhark-mode company))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
