(require 'go-mode)
(require 'key-chord)
(require 'company-go)
(require 'go-eldoc)
(require 'yasnippet)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)


 ;; jump to file
(key-chord-define go-mode-map "5t" 'godef-jump)
(key-chord-define go-mode-map "4t" 'godef-jump-other-window)


(defun my-go-mode-hook ()
  ;; customize compile command
  ;; (if (not (string-match "go" compile-command))
  ;;     (set (make-local-variable 'compile-command)
  ;;          "go vet && go test"))

  ;; go uses tabs
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (yas-minor-mode)
  
  (set (make-local-variable 'company-backends) '(company-go))        
  (company-mode) 
  (go-eldoc-setup)
  
  (local-set-key (kbd "M-.") 'godef-jump)
  )                              

(add-hook 'go-mode-hook 'my-go-mode-hook)

