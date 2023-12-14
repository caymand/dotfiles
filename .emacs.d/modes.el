;; C 
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)
;; Text
(add-hook 'text-mode-hook (lambda ()
							(auto-fill-mode )
							(flyspell-mode )
							(define-key text-mode-map "\C-c\C-a" 'artist-mode)))
;; ISPC
(use-package ispc-mode
  :ensure t)

;; Asm
(add-hook 'asm-mode-hook (lambda ()
						   (setq electric-indent-mode nil)))

;; Futhark
(add-hook 'futhark-mode-hok (lambda ()
							  (flycheck-mode)
							  (eglot-ensure)))
