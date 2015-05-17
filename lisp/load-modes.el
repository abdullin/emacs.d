
;; load all .el files inside `modules-dir`
(setq modules-dir (expand-file-name "lisp/modes" emacs-root-dir))
(mapc 'load (directory-files modules-dir 't "^[^#].*el$"))
