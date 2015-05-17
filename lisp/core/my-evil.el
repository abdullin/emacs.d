(require `evil)



(message "starting evil")
;;(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(evil-mode 1)
;;Motion state map disables the cursor keys in normal, operator, visual
;; as well as the special motion states.
;;(define-key evil-insert-state-map [left] 'undefined)
;;(define-key evil-insert-state-map [right] 'undefined)
;;(define-key evil-insert-state-map [up] 'undefined)
;;(define-key evil-insert-state-map [down] 'undefined)

;;(define-key evil-motion-state-map [left] 'undefined)
;;(define-key evil-motion-state-map [right] 'undefined)
;;(define-key evil-motion-state-map [up] 'undefined)
;;(define-key evil-motion-state-map [down] 'undefined)

