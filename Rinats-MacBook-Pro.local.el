;; enable all modes
(setq ra/roles '(
                 "org"
                 "ess"
                 "erlang"
                 ;; "go"
                 "web" ;;HTML,CSS,JS,JSX etc
                 ))

;; config for erlang on OSX
(setq ra/erlang-root-path "/usr/local/Cellar/erlang/18.0.3/")
(setq ra/erlang-tools-path "/usr/local/Cellar/erlang/18.0.3/lib/erlang/lib/tools-2.8/emacs/")
;;config for R on OSX
(setq inferior-R-program-name "/usr/local/Cellar/r/3.2.2_1/bin/R")
