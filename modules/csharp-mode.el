(add-to-list `el-get-sources 
             `(:name csharp-mode
                     :type github
                     :pkgname "abdullin/csharp-mode"
                     :build `("make")
			:build/darwin `(
( "make" ,(format "EMACS_COMMAND=%s" el-get-emacs)))
                     :features csharp-mode
                     ))
