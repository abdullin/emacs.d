(defun ra/load-unix-shell-env ()
  "Adds the shell environment variables to Emacs' process environment."
  (interactive)
  (let* ((env (shell-command-to-string "$SHELL -i -c 'printenv'"))
     (entries (split-string env "\n" t)))
    (mapc (lambda (entry)
        (add-to-list 'process-environment entry))
      entries)))

(when (eq system-type 'darwin)
  (set-default-font "MonacoB-16")
(set-fontset-font t 'cyrillic "Droid Sans Mono")
  )

(load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)
;;
;; tomorrow:
;; (load-theme 'tomorrow t)
;; (load-theme 'tomorrow-night t)
;; (load-theme 'tomorrow-night-bright t)
;; (load-theme 'tomorrow-night-blue t)
;; (load-theme 'tomorrow-night-eighties t)
;;
;; zenburn:
;; (load-theme 'zenburn t)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-style -1)
(menu-bar-mode -1)

(ra/load-unix-shell-env)

(provide 'init-client)
