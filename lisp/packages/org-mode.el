(add-to-list 'el-get-sources
             '(:name org-mode
                     :website "http://orgmode.org/"
                     :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
                     :type git
                     :url "git://orgmode.org/org-mode.git"
                     :info "doc"
                     :build/berkeley-unix `,(mapcar
                                             (lambda (target)
                                               (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                             '("oldorg"))
                     :build `,(mapcar
                               (lambda (target)
                                 (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                               '("oldorg"))
                     :load-path ("." "contrib/lisp" "lisp")
                     :load ("lisp/org-loaddefs.el")
                     :features org
                     :after (progn
                              (message "After org-mode load")
                              (setq org-export-backends (quote (
       ascii
       ;;beamer
       html
       ;;latex
       md
       ;;odt
       ;;s5
       ;;taskjuggler
)))

                              )
                     ))
