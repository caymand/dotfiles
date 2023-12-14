;; Everything related to having a decent programming environment
;; Compilation now inserts ansi escape codes

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(grep-apply-setting
 'grep-find-command
 '("rg -n -H --no-heading -e ''" . 27))

;; Code formating
(use-package format-all
  :ensure t)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(setq format-all-show-errors 'errors)
