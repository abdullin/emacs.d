;;(defun fontify-frame (frame)
;;  (set-frame-parameter frame 'font "Monospace-11"))
;; Increase size
;;(set-face-attribute 'default nil :height 160)

(setq emacs-root-dir user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory
        (convert-standard-filename "lisp/")))


(load "core/functions")
(load "core/env")
;; loads all components
(load "core/packages")


(load "core/ui")

;; various key bindings
(load "core/my-chords")
(load "core/window")
(load "core/my-evil")
(load "core/editor")

;; modes
(load "modes/my-org-mode")
(load "modes/my-js-mode")
(load "modes/my-web-mode")
(load "modes/my-doc-mode")
(load "modes/my-go-mode")
;; (load "my-csharp-mode")

(server-start)
