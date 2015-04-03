(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq eopengrok-root-path project-directory))

(add-to-list 'load-path eopengrok-root-path)

(require 'eopengrok)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*eopengrok*"))
 (erase-buffer)
 (fundamental-mode)
 (deactivate-mark))

(After)
