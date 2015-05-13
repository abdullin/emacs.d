;; set your desired tab width
(setq-default indicate-empty-lines t)
;; display tab chars as 4
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)


;;(global-linum-mode t)
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight matching parens
(show-paren-mode t)

;; highlight current line
(add-hook 'after-change-major-mode-hook 'hl-line-mode)

;; enable interactive do
(ido-mode t)

;; disable auto-save capabilities
(setq make-backup-files nil)
(setq auto-save-default nil)


;; Auto-load changes from file
(global-auto-revert-mode t)

;; let dired auto-gess
(setq dired-dwim-target t)


;; save cursor locations
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file "~/.emacs.d/saved-places")


;; Disable auto-complete almost everywhere (for now)
(require 'auto-complete)
(setq ac-modes '(sql-mode c++-mode))

(require 'yasnippet)
(setq yas-snippet-dirs (expand-file-name "snippets" emacs-root-dir))
(yas-reload-all)
