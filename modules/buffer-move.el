(add-to-list `el-get-sources
             `(:name buffer-move
       :description "Swap buffers without typing C-x b on each window"
       :type emacswiki
       :features buffer-move
       :after (progn
                 (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                 (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                 (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                 (global-set-key (kbd "<C-S-right>")  'buf-move-right)

)))
