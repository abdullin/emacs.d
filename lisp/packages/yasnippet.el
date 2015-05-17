(add-to-list 'el-get-sources
             '(:name yasnippet
       :website "https://github.com/capitaomorte/yasnippet.git"
       :description "YASnippet is a template system for Emacs."
       :type github
       :pkgname "capitaomorte/yasnippet"
       :compile "yasnippet.el"
       :features yasnippet

       ;; only fetch the `snippets' submodule, others have funny
       ;; file names that can cause problems
       ;; see https://github.com/dimitri/el-get/issues/1511
       :submodule nil
       :build (("git" "submodule" "update" "--init" "--" "snippets"))
      ))
