(provide 'init-client)

(when (eq system-type 'darwin)
  (set-default-font "MonacoB-16")
  (set-fontset-font t 'cyrillic "Droid Sans Mono")
  )

(el-get-bundle solarized-theme)
(load-theme 'solarized-light t)

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

(defhydra hydra-zoom (global-map "<f9>")
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-style -1)
(menu-bar-mode -1)
