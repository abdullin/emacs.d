;; install a cool undo tree plugin

(add-to-list 'el-get-sources 
              '(:name undo-tree
                      :features undo-tree
                      :depends key-chord
                      :after (progn
                               (key-chord-define-global "zz" 'undo-tree-visualize)
                               )))
