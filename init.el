;;(defun fontify-frame (frame)
;;  (set-frame-parameter frame 'font "Monospace-11"))
;; Increase size
;;(set-face-attribute 'default nil :height 160)

(setq emacs-root-dir user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory
        (convert-standard-filename "lisp/")))


(load "functions")
(load "env")
;; loads all components
(load "packages")


(load "ui")

;; various key bindings
(load "my-chords")
(load "window")
(load "my-evil")
(load "editor")

;; modes
(load "my-org-mode")
(load "my-js-mode")
(load "my-web-mode")
(load "my-doc-mode")
(load "my-go-mode")
;; (load "my-csharp-mode")

(server-start)
