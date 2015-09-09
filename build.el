(require 'org)         ;; The org-mode goodness
(require 'ob)          ;; org-mode export system
(require 'ob-tangle)   ;; org-mode tangling process




(defun ra/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)   ;;  (expand-file-name file \"$DIR\")
  (org-babel-tangle)
  (kill-buffer))



(defun ra/tangle-files (path)
  "Given a directory, PATH, of 'org-mode' files, tangle source code out of all literate programming files."
  (interactive "D")
  (mapc 'ra/tangle-file (ra/get-files path)))

(ra/build-dot-files)  ;; Do it

(provide 'dot-files)
