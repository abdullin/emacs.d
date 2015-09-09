(provide 'init-packages)

(add-to-list 'load-path (expand-file-name "el-get/el-get" emacs-root-dir))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun ra/install-missing-packages()
  (el-get 'sync (mapcar 'el-get-source-name el-get-sources))
)

;; load all .el files inside `modules-dir`
(setq modules-dir (expand-file-name "packages" emacs-root-dir))
(mapc 'load (directory-files modules-dir 't "^[^#].*el$"))

;; VIM emulation
(el-get-bundle evil)
(el-get-bundle dired-plus)
(el-get-bundle flycheck)
(el-get-bundle yasnippet)
;; latest version of org-mode
(el-get-bundle org)
(el-get-bundle key-chord)
;; undo tree git-style
(el-get-bundle undo-tree) 
;; Swap buffers without typing C-x b on each window
(el-get-bundle buffer-move)  
;; smart region expansion
(el-get-bundle expand-region)

(el-get-bundle web-mode)

(el-get-bundle solarized-theme)

(el-get-bundle markdown-mode)

(el-get-bundle linum-relative
  (setq linum-relative-current-symbol "")
  )

(el-get-bundle paredit
  (add-hook-list 'paredit-mode lisp-mode-hooks)
  )
(el-get-bundle rainbow-delimeters
  (add-hook-list 'rainbow-delimiters-mode lisp-mode-hooks)
  )
