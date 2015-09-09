(provide 'init-client)

(when (eq system-type 'darwin)
  (set-default-font "MonacoB-16")
  (set-fontset-font t 'cyrillic "Droid Sans Mono")
  )

(el-get-bundle solarized-theme
  )

  (load-theme 'solarized-light t)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-style -1)
(menu-bar-mode -1)
