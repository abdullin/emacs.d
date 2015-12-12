(when
    (boundp 'ra/erlang-root-path)

  (setq erlang-root-dir (concat ra/erlang-root-path "/lib/erlang/lib"))
  (setq erlang-bin-path (concat ra/erlang-root-path "/lib/erlang/bin"))
  (setq erlang-emacs-path ra/erlang-tools-path)

  (setq load-path (cons erlang-emacs-path load-path))
  (setq exec-path (cons erlang-bin-path exec-path))
  (require 'erlang-start)

  )

(provide 'init-erlang)
