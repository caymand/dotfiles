(provide 'lib)

(defun load-init-file ()
  (interactive)
  (find-file-other-window user-init-file))

(defun my-theme () 
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
  (set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
  (set-face-attribute 'font-lock-function-call-face nil :foreground "peru")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
  (set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
  ;; Be careful. Do you really want to have a color for this
  (set-face-attribute 'font-lock-type-face nil :foreground "sienna")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
  (set-face-attribute 'font-lock-number-face nil :foreground "sienna")
  (global-font-lock-mode t)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "Gray15"))


