(add-to-list 'el-get-sources
             '(:name go-mode
                     :type github
                     :description "Go mode"
                     :depends key-chord
                     :pkgname "dominikh/go-mode.el"
                     :features go-mode
                     ))

(add-to-list 'el-get-sources
             '(:name gocode
                     :depends (company-mode go-mode)
                     :type github
                     :description "Gocode"
                     :pkgname "nsf/gocode"
                     :load "emacs-company/company-go.el"
                     :features company-go
                     ))


(add-to-list 'el-get-sources
             '(:name go-eldoc
                     :description "eldoc plugin for Go"
                     :type github
                     :pkgname "syohex/emacs-go-eldoc"
                     :depends go-mode
                     :features go-eldoc
                     ))

