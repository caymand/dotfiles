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
(my-theme)
(setq-default indent-tabs-mode nil)
(setq grep-command "rg --no-heading -nH ")
(setq grep-find-command "rg --no-heading --hidden ")
(setq column-number-mode t)
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Iosevka" :height 160))
(vertico-mode)
(setq split-width-threshold 160)

;; KEYS
(keymap-global-set "C-c c c" 'compile)

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
 '(custom-enabled-themes nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   '(magit dumb-jump ormolu vertico which-key markdown-mode haskell-mode futhark-mode company))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
