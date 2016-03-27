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

(add-to-list 'exec-path "/usr/local/bin/")

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

;; modern list library
(el-get-bundle dash)
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

(require 'dash)

(el-get-bundle key-chord)
(require 'key-chord)
(key-chord-mode 1)

;; I know what the scratch is for
(setq initial-scratch-message "")

;; don't show the startup help screen
(setq inhibit-startup-screen t)

;; disable alarm bell beep
(setq visible-bell t)
;; flash on OSX looks ugly
(setq ring-bell-function 'ignore)

;; move to a neighbor window using SUPER + VIM KEY
(require 'windmove)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)

(global-set-key (kbd "s-S-H") 'buf-move-left)
(global-set-key (kbd "s-S-J") 'buf-move-down)
(global-set-key (kbd "s-S-K") 'buf-move-up)
(global-set-key (kbd "s-S-L") 'buf-move-right)

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
  ("<left>"  hydra-move-splitter-left)
  ("<down>" hydra-move-splitter-down)
  ("<up>" hydra-move-splitter-up)
  ("<right>" hydra-move-splitter-right)
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

;; get smart-mode-line
(el-get-bundle smart-mode-line)
;; respect the current theme
(setq sml/theme 'respectful)
;; don't ask for confirmation
(setq sml/no-confirm-load-theme t)
;; taken from Sasha Chua
(setq-default
   mode-line-format
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     mode-line-buffer-identification
     "  "
     mode-line-position
     (vc-mode vc-mode)
     "  "
     mode-line-modes
     mode-line-misc-info
     mode-line-end-spaces))

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

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
                  ;;("perl" (mode . cperl-mode))
                  ;;("erc" (mode . erc-mode))
               ("org" (or
                       (mode . org-mode)
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (name . "^\\.org$")
                       (mode . muse-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         ))

               ("clojure" (or
                           (mode . clojure-mode)
                           (name . "^cider")
                           ))
                  ;; ("gnus" (or
                  ;;          (mode . message-mode)
                  ;;          (mode . bbdb-mode)
                  ;;          (mode . mail-mode)
                  ;;          (mode . gnus-group-mode)
                  ;;          (mode . gnus-summary-mode)
                  ;;          (mode . gnus-article-mode)
                  ;;          (name . "^\\.bbdb$")
                  ;;          (name . "^\\.newsrc-dribble")))
                  ))))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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
  ("b" ibuffer "buf")
  )

(key-chord-define-global ";'" 'hydra-jump/body)

;; just follow symlink and open the actual file
(setq vc-follow-symlinks t)

;; latest version of org-mode
(el-get-bundle org-mode)
(require 'org)

(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-odd-level-only t)
(setq org-indent-mode t)

(setq org-startup-with-inline-images t)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(setq org-directory "~/org")

(defun ra/remove-lock-files (fs)
  "Removes file names matching .# pattern (emacs lock files"
  (-remove(lambda (x) (string-match "\.#" x)) fs)
  )

(defun ra/list-possible-org-files ()
  "Provides a list of all matching org files"
  (ra/remove-lock-files
   (append
    (file-expand-wildcards "~/org/*.org")       ;; core org files
    (file-expand-wildcards "~/org/links/*.org") ;; linked org files
    ;;(file-expand-wildcards "~/proj/*/*.org")
    ;;(file-expand-wildcards "~/proj/*/org/*.org")
    )
   )
  )

(setq org-agenda-files (ra/list-possible-org-files))

(setq org-completion-use-ido t)

(global-set-key "\C-cb" 'org-iswitchb)

(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

(define-key org-mode-map  (kbd "C-c w") 'hydra-org-clock/body)

;; wire up
(require 'org-id)
;; Create if storing link interactively and no CUSTOM_ID is present
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(define-key org-mode-map  (kbd "C-c l") 'org-store-link)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;;; color keywords
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              )))

;; Changing a task state is done with =C-C C-t KEY=:
(setq org-use-fast-todo-selection t)
;; changing states with S + arrow does not trigger full change
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-default-notes-file "~/org/inbox.org")
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (
              ("i" "index" entry (file+datetree "~/org/index.org")
               "* INDEX: %?") 
              )))

(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-agenda-custom-commands
      '(
        ;; ("X" agenda "" nil ("agenda.html" "agenda.ps"))
        ;; ("Y" alltodo "" nil ("todo.html" "todo.txt" "todo.ps"))
        ;; ("h" "Agenda and Home-related tasks"
        ;;  ((agenda "")
        ;;   (tags-todo "home")
        ;;   (tags "garden"))
        ;;  nil
        ;;  ("~/views/home.html"))


        ("F" "full agenda view"
         ((agenda ""
                  ;; array of constraints
                  (
                   ;; next 30 days
                   (org-agenda-ndays 30)
                   ;; drop empty blocks
                   (org-agenda-show-all-dates nil)
                   ))
          ;; agenda command options
          ;;(tags-todo "work")
          ;;(tags "office")
          )
         nil
         (
          "~/org/views/agenda_full.ps"
          "~/org/views/agenda_full.ics"
          "~/org/views/agenda_full.html"
          ))
        ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (gnuplot . t)
   (clojure . t)
   ))

(setq org-confirm-babel-evaluate nil)

(setq org-src-window-setup 'current-window)

(defun ra/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'ra/fix-inline-images)

(setq org-export-backends (quote (
       ascii
       ;;beamer
       html
       ;;latex
       md
       ;;odt
       ;;s5
       ;;taskjuggler
)))

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

(el-get-bundle ess)
(el-get-bundle gnuplot-mode)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

(el-get-bundle spinner)
(el-get-bundle clojure-mode)
(el-get-bundle cider)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(provide 'init-main)
