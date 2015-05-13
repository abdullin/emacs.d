(setq lisp-mode-hooks '(emacs-lisp-mode-hook
			lisp-mode-hook
			lisp-interaction-mode-hook
			scheme-mode-hook
			clojure-mode-hook))

(add-to-list 'el-get-sources
	     '(:name paredit
		     :after (add-hook-list 'paredit-mode lisp-mode-hooks)))

(add-to-list 'el-get-sources
	     '(:name rainbow-delimiters
		     :after (add-hook-list 'rainbow-delimiters-mode lisp-mode-hooks)))
