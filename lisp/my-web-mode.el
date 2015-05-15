(require 'web-mode)
(require 'yasnippet)
(require 'company)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;; activate JSX mode
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  ; YAS has to be loaded before company
  (yas-minor-mode 1)
  (setq indent-tabs-mode t)

  (company-mode)


  (add-hook 'before-save-hook 'whitespace-cleanup)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; auto-completion sources


 (setq web-mode-ac-sources-alist
       '(
         ("html" . (ac-source-yasnippet))
         ))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "html")
                   (yas-activate-extra-mode 'html-mode)
                 (yas-deactivate-extra-mode 'html-mode))
               )))





(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; JSX syntax checking

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
;; We need to use source-inplace because eslint looks for
            ;; configuration files in the directory of the file being checked.
            ;; See https://github.com/flycheck/flycheck/issues/447
  :command ("eslint" "--format=checkstyle" source-inplace)
   :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (mapc (lambda (err)
                          ;; Parse error ID from the error message
                          (setf (flycheck-error-message err)
                                (replace-regexp-in-string
                                 (rx " ("
                                     (group (one-or-more (not (any ")"))))
                                     ")" string-end)
                                 (lambda (s)
                                   (setf (flycheck-error-id err)
                                         (match-string 1 s))
                                   "")
                                 (flycheck-error-message err))))
                        (flycheck-sanitize-errors errors))
                  errors)
  :modes (web-mode)
  )


(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode)
              ;;
              (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'jsx-mode)))



              )))
