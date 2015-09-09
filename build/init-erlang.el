(when
    (boundp 'ra/erlang-path)

  (setq erlang-root-dir (concat ra/erlang-path "/lib/erlang/lib"))
  (setq erlang-emacs-path (concat ra/erlang-path "/lib/erlang/lib/tools-2.7.2/emacs"))
  (setq erlang-bin-path (concat ra/erlang-path "/lib/erlang/bin"))
  (setq load-path (cons erlang-emacs-path load-path))
  (setq exec-path (cons erlang-bin-path exec-path))
  (require 'erlang-start)
 
  )

(provide 'init-erlang)
