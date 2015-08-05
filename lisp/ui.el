;; copied from https://github.com/danielfm/dotfiles-emacs/blob/master/ui.el

;; don't show the startup help screen
(setq inhibit-startup-screen t)

;; mode line settings
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;; remove clutter from the UI
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-style -1)
(menu-bar-mode -1)
;; blinking cursor
(blink-cursor-mode t)

;; disable alarm bell beep
(setq visible-bell t)

;; use Monaco font in Mac OS X
(when (eq system-type 'darwin)
  (set-default-font "MonacoB-16")
(set-fontset-font t 'cyrillic "Droid Sans Mono")
  )

;; my custom theme of choice
;;(load-theme 'tomorrow-night-bright t)

;; available custom themes
;;
;; solarized:

(load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)
;;
;; tomorrow:
;; (load-theme 'tomorrow t)
;; (load-theme 'tomorrow-night t)
;; (load-theme 'tomorrow-night-bright t)
;; (load-theme 'tomorrow-night-blue t)
;; (load-theme 'tomorrow-night-eighties t)
;;
;; zenburn:
;; (load-theme 'zenburn t)


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
