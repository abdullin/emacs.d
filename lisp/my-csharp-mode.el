(require 'company)
(require 'omnisharp)
(require 'flycheck)
(require 'key-chord)
(require 'eldoc)


(defun my-csharp-mode-hook ()

  (flycheck-mode)
  (omnisharp-mode)

  (eldoc-mode)


  (set (make-local-variable 'company-backends) '(company-omnisharp))
  (company-mode)

  (key-chord-define omnisharp-mode-map "5t" 'omnisharp-go-to-definition)
)

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
