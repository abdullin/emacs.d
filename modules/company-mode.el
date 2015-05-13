(add-to-list 'el-get-sources
             '(:name company-mode
                     :features company
                     :depends key-chord
                     :after (progn
                              ;; enable company mode globally
                                        ; (add-hook 'after-init-hook 'global-company-mode)
                                        ;(company-mode)
                                        ;(message "Company mode registered hook")
                              (defun my/company-complete-lambda (arg)
                                "Ignores passed in arg like a lambda and runs company-complete"
                                (company-complete))


                              ;; bigger popup window
                              (setq company-tooltip-limit 20)
                              ;; autocomplete right after '.'
                              (setq company-minimum-prefix-length 0)
                              ;; disable automatic completion
                              (setq company-idle-delay nil)
                              ;; removes annoying blinking
                              (setq company-echo-delay 0)
                              ;; start autocompletion only after typing
                                        ;(setq company-begin-commands '(self-insert-command))
                              
                              (setq
                               ;; make sure evil uses the right completion functions
                               evil-complete-next-func 'my/company-complete-lambda
                               evil-complete-previous-func 'my/company-complete-lambda

                               )


                              (global-set-key (kbd "C-\\") 'company-complete)

                              
                              (key-chord-define-global ";m" 'company-complete)
                              )))

;(add-hook-list 'after-init-hook 'global-company-mode)
; load company for everybody

