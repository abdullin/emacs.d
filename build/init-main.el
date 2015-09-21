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

;; load all .el files inside `modules-dir`
(setq modules-dir (expand-file-name "packages" emacs-root-dir))
(mapc 'load (directory-files modules-dir 't "^[^#].*el$"))

(el-get-bundle dired-plus)
(el-get-bundle flycheck)
(el-get-bundle yasnippet)
(el-get-bundle company)
;; Swap buffers without typing C-x b on each window
(el-get-bundle buffer-move)  
(el-get-bundle hydra)
(el-get-bundle web-mode)
;; undo tree git-style
(el-get-bundle undo-tree)

(el-get-bundle key-chord)
(require 'key-chord)
(key-chord-mode 1)

;; I know what the scratch is for
(setq initial-scratch-message "")

;; don't show the startup help screen
(setq inhibit-startup-screen t)

;; disable alarm bell beep
(setq visible-bell t)

;; move to a neighbor window using SHIFT-<arrow-key>
(require 'windmove)

;;(windmove-default-keybindings)

(winner-mode 1)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra ra/hydra-windows (global-map "<f2>")
  "winops"
  ("SPC" nil)
  ("<left>" windmove-left :color blue)
  ("<down>" windmove-down :color blue) 
  ("<up>" windmove-up :color blue)
  ("<right>" windmove-right :color blue)
  ("h" windmove-left :color blue)
  ("j" windmove-down :color blue) 
  ("k" windmove-up :color blue)
  ("l" windmove-right :color blue)
  ("S-<left>" buf-move-left)
  ("S-<down>" buf-move-down)
  ("S-<up>" buf-move-up)
  ("S-<right>" buf-move-right)
  ("x" delete-window :color blue)
  ("X" delete-other-windows :color blue)
  ("z" (progn
        (winner-undo)
        (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("r" split-window-right :color blue)
  ("b" split-window-below :color blue)
  )

(el-get-bundle smart-mode-line)
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)

(display-battery-mode)

;; blinking cursor
(blink-cursor-mode t)

(require 'init-client)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(setq dired-dwim-target t)

(el-get-bundle markdown-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode)
             )

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun ra/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'ra/unfill-paragraph)

;; smart region expansion
(el-get-bundle expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; VIM emulation
(el-get-bundle evil)
(require 'evil)
(evil-mode 1)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jj" 'evil-normal-state)
(key-chord-define evil-normal-state-map "jj" 'evil-normal-state)

(show-paren-mode t)

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

(defun ra/kill-this-buffer-if-not-modified ()
  (interactive)
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))

(defhydra hydra-jump (:color blue)
  "jumps"
  ("d" dired-jump "dired")
  ("." ido-find-file "file")
  ("l" ido-switch-buffer "buffer")
  ("k" ra/kill-this-buffer-if-not-modified "kill")
  ("z" undo-tree-visualize "undo")
  (";" execute-extended-command "meta-x")
  ("w" ra/hydra-windows/body "win")
  )

(key-chord-define-global ";'" 'hydra-jump/body)

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

(global-set-key (kbd "<escape>")      'nil)

(el-get-bundle linum-relative
  (setq linum-relative-current-symbol "")
  )

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

(el-get-bundle paredit
  (add-hook-list 'paredit-mode lisp-mode-hooks)
  )
(el-get-bundle rainbow-delimiters
  (add-hook-list 'rainbow-delimiters-mode lisp-mode-hooks)
  )

(require 'init-web)

(require 'init-org)

(el-get-bundle ess)
(el-get-bundle gnuplot-mode)

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

(provide 'init-main)
