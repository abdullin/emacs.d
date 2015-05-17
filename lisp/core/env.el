;; loads the shell environment variables when launching Emacs outside shell
(when (and window-system
	   (in-list-p system-type '(darwin gnu/linux)))
  (load-unix-shell-env))
