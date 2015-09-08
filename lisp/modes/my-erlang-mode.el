(setq brew-erlang-path "/usr/local/Cellar/erlang/17.5/")

(setq erlang-root-dir (concat brew-erlang-path "/lib/erlang/lib"))
(setq erlang-emacs-path (concat brew-erlang-path "/lib/erlang/lib/tools-2.7.2/emacs"))
(setq erlang-bin-path (concat brew-erlang-path "/lib/erlang/bin"))

(setq load-path (cons erlang-emacs-path load-path))
(setq exec-path (cons erlang-bin-path exec-path))

(require 'erlang-start)
