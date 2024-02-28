;; Vars
(defvar todo-dir "~/todo/")
(defvar todo-file (concat todo-dir "todo.md"))

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


(defun create-local-keys (&rest key-cmds)
  (when key-cmds
	(let ((key (car key-cmds))			
		  (cmd (cadr key-cmds)))
	  (apply 'create-local-keys (cddr key-cmds)))))
  
(defun selected-text (start end)
  ;; this means that start and end will be the mark of the start and end string
  (interactive "r") 
  (buffer-substring-no-properties start end))

(defun load-init-file ()
  (interactive)
  (find-file-other-window user-init-file))


;; Visual
(defun my-theme () 
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
  (set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
  ;;(set-face-attribute 'font-lock-function-call-face nil :foreground "peru")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
  (set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
  ;; Be careful. Do you really want to have a color for this
  (set-face-attribute 'font-lock-type-face nil :foreground "sienna")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
  (global-font-lock-mode t)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "Gray15"))


