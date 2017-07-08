(provide 'init-client)

(when (eq system-type 'darwin)
  ;; set default font for the frames as well (daemon + ec)
  (setq default-frame-alist '((font . "MonacoB-14")))
  (set-fontset-font t 'cyrillic "Droid Sans Mono")
  )

(el-get-bundle color-theme-solarized)
(load-theme 'solarized t)

(defhydra hydra-themes (global-map "<f9>")
  "themes"
  ("SPC" nil)
  ("q"
   (lambda ()
     (interactive)
     (load-theme 'solarized-light t)
     )
   )
  ("w"
   (lambda ()
     (interactive)
     (load-theme 'solarized-dark t)
     )
   )
  )

(defhydra hydra-zoom (global-map "<f6>")
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))
