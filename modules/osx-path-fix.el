(add-to-list 'el-get-sources
                  '(:name exec-path-from-shell
:features  exec-path-from-shell
:after (when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
))



