;; http://flycheck.readthedocs.org/en/latest/guide/installation.html
;; enable via M-x flycheck-mode

(add-to-list `el-get-sources
             `(
               :name flycheck
                     :type github
                     :pkgname "flycheck/flycheck"
                     :description "On-the-fly syntax checking extension"
                     :build '(("makeinfo" "-o" "doc/flycheck.info" "doc/flycheck.texi"))
                     :info "./doc"
                     :depends (s dash cl-lib f pkg-info)
                     )             
             )





