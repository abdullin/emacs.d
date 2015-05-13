;; move to a neighbor window using SHIFT-<arrow-key>
(windmove-default-keybindings)

;; don't conflict with orgmode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)





;; S-up does not work properly in terminals
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html
 (if (equal "xterm" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up]))

(defadvice terminal-init-xterm (after select-shift-up activate)
    (define-key input-decode-map "\e[1;2A" [S-up]))

;; enlarge and shrink windows
(global-set-key (kbd "C-c <up>")    'shrink-window)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c <down>")  'enlarge-window)
(global-set-key (kbd "C-c <left>")  'shrink-window-horizontally)
