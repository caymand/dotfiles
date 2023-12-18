;; Vars
(defvar todo-dir "~/todo/")
(defvar todo-file (concat todo-dir "todo.md"))

(defun create-local-keys (&rest key-cmds)
  (when key-cmds
	(let ((key (car key-cmds))			
		  (cmd (cadr key-cmds)))
	  (apply 'create-local-keys (cddr key-cmds)))))
  
(defun selected-text (start end)
  ;; this means that start and end will be the mark of the start and end string
  (interactive "r") 
  (buffer-substring-no-properties start end))




