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

(el-get-bundle evil)
(el-get-bundle dired-plus)
(el-get-bundle flycheck)
(el-get-bundle yasnippet)

(el-get-bundle markdown-mode)

(el-get-bundle linum-relative
  (setq linum-relative-current-symbol "")
  )
