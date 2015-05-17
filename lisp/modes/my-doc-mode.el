;; ASCI-DOC
(require `doc-mode) 
;; good writing tips
(require `writegood-mode)


(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(autoload 'doc-mode "doc-mode")

(defun my-doc-mode-hook ()
	(writegood-mode)
)

(add-hook 'doc-mode-hook 'my-doc-mode-hook)
