(require `company)
(require `flycheck)
(require `yasnippet)
(require `jsfmt)
;;(add-hook `js-mode-hook `flycheck-mode)
;;(add-hook `js-mode-hook `company-mode)

(add-hook 'before-save-hook 'jsfmt-before-save)
(add-hook 'js-mode-hook
          (lambda ()
            ;; activate on-the-fly-check (will use installed linter)
;;            (flycheck-mode)
            ;; auto-completion
            (company-mode)
            ;; Activate the folding mode
;;            (hs-minor-mode t)
            ;; snippets
            (yas-minor-mode)


            ;; perform flycheck on save
            (setq flycheck-check-syntax-automatically '(save))
            ;; run flycheck here
            (flycheck-mode)

            )
          )


(setq js-indent-level 4)

(provide 'init-js)
