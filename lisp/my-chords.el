(require `key-chord)



(key-chord-mode 1)
(key-chord-define-global ";s" 'save-buffer)

;; Window management
;;(key-chord-define-global ";0" 'delete-window)
;;(key-chord-define-global ";1" 'delete-other-windows)
;;(key-chord-define-global ";2" 'split-window-below)
;;(key-chord-define-global ";3" 'split-window-right)
;; buffers
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
