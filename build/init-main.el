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

(defun ra/kill-this-buffer-if-not-modified ()
  (interactive)
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))

(defun ra/save-and-recompile()
  (interactive)
  (save-buffer)
  (recompile)
  )

(require 'init-packages)
(ra/install-missing-packages)

;; I know what the scratch is for
(setq initial-scratch-message "")

;; don't show the startup help screen
(setq inhibit-startup-screen t)

;; disable alarm bell beep
(setq visible-bell t)

;; move to a neighbor window using SHIFT-<arrow-key>
(windmove-default-keybindings)

;; don't conflict with orgmode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(when (window-system)
  (require 'init-client)
  )

(setq dired-dwim-target t)

(add-to-list 'auto-mode-alist
             '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode)
             )

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

(require 'key-chord)
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

(key-chord-define-global "zz" 'undo-tree-visualize) ;; open undo-tree

(key-chord-define-global ";k"     'ra/kill-this-buffer-if-not-modified)


;; SAVE

(global-set-key (kbd "<f2>") `ra/save-and-recompile)
(global-set-key (kbd "<f8>") `recompile)
(global-set-key (kbd "<f9>") `next-error)

;(key-chord-define-global "e2" 'er/contract-region)
(key-chord-define-global "4r" 'er/expand-region)

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

(defun ra/load-unix-shell-env ()
  "Adds the shell environment variables to Emacs' process environment."
  (interactive)
  (let* ((env (shell-command-to-string "$SHELL -i -c 'printenv'"))
     (entries (split-string env "\n" t)))
    (mapc (lambda (entry)
        (add-to-list 'process-environment entry))
      entries)))

(ra/load-unix-shell-env)

(require 'init-erlang)

(require 'init-js)

(setq lisp-mode-hooks '(emacs-lisp-mode-hook
            lisp-mode-hook
            lisp-interaction-mode-hook
            scheme-mode-hook
            clojure-mode-hook))

(require 'init-web)

(require 'init-org)

(provide 'init-main)
