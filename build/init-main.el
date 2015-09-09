(defconst ra/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun ra/emacs-subdirectory (d) (expand-file-name d ra/emacs-directory))

(setq emacs-root-dir user-emacs-directory)

(setq ra/emacs-machine-init
      (expand-file-name (concat system-name ".el") ra/emacs-directory)
      )

;; (setq custom-file (expand-file-name "init-local.el" ra/emacs-directory))
(when (file-exists-p ra/emacs-machine-init)
  (load ra/emacs-machine-init))

(add-to-list 'load-path (ra/emacs-subdirectory "lisp"))
(add-to-list 'load-path (ra/emacs-subdirectory "build"))

(defun add-hook-list (callback hooks)
  "Adds callback to each one of the hooks."
  (mapc (lambda (hook)
      (add-hook hook callback))
    hooks))

(add-to-list 'load-path (expand-file-name "el-get/el-get" emacs-root-dir))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun ra/require-el-get (package)
  "Add a package to el-get-sources"
  (add-to-list 'el-get-sources package))

(ra/require-el-get 'evil)
(ra/require-el-get 'dired-plus)

(setq linum-relative-current-symbol "")
(ra/require-el-get 'linum-relative)

;; load all .el files inside `modules-dir`
(setq modules-dir (expand-file-name "packages" emacs-root-dir))
(mapc 'load (directory-files modules-dir 't "^[^#].*el$"))

;; install all missing packages via el-get
(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

;; I know what the scratch is for
(setq initial-scratch-message "")

;; don't show the startup help screen
(setq inhibit-startup-screen t)

;; disable alarm bell beep
(setq visible-bell t)

(when (window-system)
  (require 'init-client)
  )

;; move to a neighbor window using SHIFT-<arrow-key>
(windmove-default-keybindings)

;; don't conflict with orgmode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq dired-dwim-target t)

(require `evil)
;;(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(evil-mode 1)

(show-paren-mode t)

;; blinking cursor
(blink-cursor-mode t)

(add-hook 'ido-setup-hook (lambda ()
                (setq ido-enable-flex-matching t)))


; Use IDO for both buffer and file completion and ido-everywhere to t
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)


(ido-mode t)

(global-auto-revert-mode t)

(set-language-environment "UTF-8")

(require 'yasnippet)
(yas-global-mode)

(setq yas-snippet-dirs (ra/emacs-subdirectory "snippets"))

(require `key-chord)
(key-chord-mode 1)

(key-chord-define-global ";b" 'ibuffer)


(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))

(defun view-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

(key-chord-define-global "5t" 'find-tag-default)

(key-chord-define-global "4t" 'view-tag-other-window)

(key-chord-define-global ";d" 'dired-jump)

(key-chord-define-global ";'" 'execute-extended-command) ;; Meta-X

(key-chord-define-global ";l" 'ido-switch-buffer)
(key-chord-define-global ";." 'ido-find-file) ;; jump to file

(defun kill-this-buffer-if-not-modified ()
  (interactive)
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))
(key-chord-define-global ";k"     'kill-this-buffer-if-not-modified)


;; SAVE
(defun save-and-recompile()
  (interactive)
  (save-buffer)
  (recompile)
  )

(global-set-key (kbd "<f2>") `save-and-recompile)
(global-set-key (kbd "<f8>") `recompile)
(global-set-key (kbd "<f9>") `next-error)

;; mode line settings
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;; set your desired tab width
(setq-default indicate-empty-lines t)

;; display tab chars as 4
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-linum-mode 1)

(defconst linum-mode-excludes '(
                                doc-view-mode
                                compilation-mode
                                term-mode
                                dired-mode
                                ibuffer-mode
                                eshell-mode
                                )
  "List of major modes preventing linum to be enabled in the buffer.")

(defadvice linum-mode (around linum-mode-selective activate)
  "Avoids enabling of linum-mode in the buffer having major mode set to one
of listed in `linum-mode-excludes'."
  (unless (member major-mode linum-mode-excludes)
    ad-do-it))

;; highlight current line
(add-hook 'after-change-major-mode-hook 'hl-line-mode)

(require 'init-erlang)

(require 'init-js)

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

(provide 'init-go)

(require 'init-web)

(require 'init-org)

(provide 'init-main)
