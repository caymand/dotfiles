(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Layout
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
(set-face-attribute 'default nil :weight 'Regular :font "Iosevka-14")
(global-linum-mode 't)
(column-number-mode 't)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Dired
(setq dired-lising-switches "-aBhl --sort=time")

;; Modes
(use-package haskell-mode
  :ensure t
)

;; Code formating
(use-package format-all
  :ensure t
)
(add-hook 'haskell-mode-hook 'format-all-mode)
(set-fill-column 81)
(auto-fill-mode 't)

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

(global-unset-key (kbd "M-1"))
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(global-unset-key (kbd "M-4"))
(global-unset-key (kbd "M-5"))
;; (global-unset-key (kbd "\C-c b p"))
;; (global-unset-key (kbd "\C-c b n"))

(global-set-keys
 "\C-c b p" 'previous-buffer
 "\C-c b n" 'next-buffer
)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(flycheck which-key use-package format-all)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
