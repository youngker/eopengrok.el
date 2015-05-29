;;; eopengrok.el --- opengrok interface for emacs

;; Copyright (C) 2015 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (magit "20150320.1639")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; opengrok interface for emacs

;;; Code:

(require 's)
(require 'dash)
(require 'magit)
(require 'cl-lib)

(defvar eopengrok-pending-output nil)
(defvar eopengrok-last-filename nil)
(defvar eopengrok-search-text nil)
(defvar eopengrok-first-match-point nil)
(defvar eopengrok-start-time nil)

(defconst eopengrok-buffer "*eopengrok*")
(defconst eopengrok-indexing-buffer "*eopengrok-indexing-buffer*")

(defconst eopengrok-text-regexp
  "\\(^/[^ ]*?\\):\\([0-9]+\\)[ \t]+\\[\\(.*\\)\\]")

(defconst eopengrok-file-regexp
  "\\(^/[^ ]*?\\):\\(\\)[ \t]+\\[\\(\\.\\.\\.\\)\\]")

(defconst eopengrok-history-regexp
  "\\(^/[^ ]*?\\):[ \t]+\\[\\(\\w+\\)[ \t]\\(.*\\)\\]")

(defcustom eopengrok-jar
  "/home/youngjooez.lee/Projects/opengrok-0.12.1.5/lib/opengrok.jar"
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-ctags
  "/usr/local/bin/ctags"
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-database
  ".opengrok/configuration.xml"
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-indexer-suffixes
  '("*.c" "*.h" "*.cpp" "*.hpp" "*.cc" "*.hh" "*.m" "*.xml" "*.mk" "*.py"
    "*.rb" "*.java" "*.js" "*.el")
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-ignored-dir
  '("CVS" "RCS" "SCCS" ".hg" ".bzr" ".cdv" ".pc" ".svn" "_MTN" "_darcs"
    "_sgbak" "debian" ".opengrok" "out" ".repo")
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-abbreviate-filename 80
  "DOC."
  :group 'eopengrok)

(defface eopengrok-file-face
  '((t :inherit font-lock-function-name-face))
  "Face for files."
  :group 'eopengrok)

(defface eopengrok-info-face
  '((t :inherit font-lock-constant-face))
  "Face for info."
  :group 'eopengrok)

(defface eopengrok-source-face
  '((t :inherit font-lock-doc-face))
  "Face for source."
  :group 'eopengrok)

(defface eopengrok-highlight-face
  '((t :inherit highlight))
  "Face for highlight item."
  :group 'eopengrok)

(defun eopengrok-switch-to-buffer ()
  (interactive)
  (when (get-buffer eopengrok-buffer)
    (pop-to-buffer eopengrok-buffer)))

(defun eopengrok-say-yes ()
  (interactive)
  (process-send-string (get-process "eopengrok") "n\n"))

(defun eopengrok-index-option-list (dir)
  (-flatten (list "-Xmx2048m"
                  "-cp" eopengrok-jar "org.opensolaris.opengrok.index.Indexer"
                  "-c" eopengrok-ctags
                  "-W" (concat dir eopengrok-database)
                  "-d" (concat dir ".opengrok")
                  "-s" dir
                  "-C" "-S" "-H"
                  (--mapcat (list "-I" it) eopengrok-indexer-suffixes)
                  (--mapcat (list "-i" it) eopengrok-ignored-dir))))

(defun eopengrok-search-configuration ()
  (let ((dir (expand-file-name default-directory)))
    (catch 'done
      (while dir
        (when (file-exists-p (concat dir eopengrok-database))
          (throw 'done (concat dir eopengrok-database)))
        (setq dir (file-name-as-directory
                   (file-name-directory
                    (directory-file-name dir))))
        (when (string= dir "/")
          (error "can't find a configuration.xml"))))))

(defun eopengrok-search-option-list (dir find-option text)
  (list "-Xmx2048m"
        "-cp" eopengrok-jar "org.opensolaris.opengrok.search.Search"
        "-R" dir find-option text))

(defmacro eopengrok-properties-region (props &rest body)
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun eopengrok-get-properties (pos)
  (list (get-text-property pos :name)
        (get-text-property pos :info)))

(defun eopengrok-show-source ()
  (with-current-buffer eopengrok-buffer
    (-when-let* (((file number) (eopengrok-get-properties (point))))
      (let* ((buffer (find-file-noselect file))
             (window (display-buffer buffer)))
        (set-buffer buffer)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- number)))
        (set-window-point window (point))
        window))))

(defun eopengrok-show-commit ()
  (-when-let* (((file commit-id) (eopengrok-get-properties (point))))
    (setq default-directory (file-name-directory file))
    (magit-git-string "rev-parse" "--show-toplevel")
    (magit-show-commit commit-id t)))

(defun eopengrok-jump-to-source ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (-when-let (info (get-text-property (point) :info))
      (if (numberp info)
          (-when-let (window (eopengrok-show-source))
            (select-window window)
            (ring-insert find-tag-marker-ring (point-marker)))
        (progn
          (eopengrok-show-commit)
          (-when-let (window (display-buffer "*magit-commit*"))
            (select-window window)))))))

(defun eopengrok-number-p (str)
  (< (length str) 8))

(defun eopengrok-next-line ()
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (next-single-property-change
                     (save-excursion (end-of-line) (point)) :info))
      (goto-char pos)
      (if (numberp (get-text-property pos :info))
          (eopengrok-show-source)
        (eopengrok-show-commit)))))

(defun eopengrok-previous-line ()
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (previous-single-property-change
                     (save-excursion (beginning-of-line) (point)) :info))
      (goto-char pos)
      (beginning-of-line)
      (if (numberp (get-text-property (point) :info))
          (eopengrok-show-source)
        (eopengrok-show-commit)))))

(defun eopengrok-abbreviate-file (file)
  (let* ((start (- (point) (length file)))
         (end (point))
         (amount (if (numberp eopengrok-abbreviate-filename)
                     (- (- end start) eopengrok-abbreviate-filename)
                   999))
         (advance-word (lambda ()
                         "Return the length of the text made invisible."
                         (let ((wend (min end (progn (forward-word 1) (point))))
                               (wbeg (max start (progn (backward-word 1) (point)))))
                           (goto-char wend)
                           (if (<= (- wend wbeg) 1)
                               0
                             (put-text-property (1+ wbeg) wend 'invisible t)
                             (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (decf amount (funcall advance-word)))
    (goto-char end)))

(defun eopengrok-remove-html-tags (line)
  (->> line
       (replace-regexp-in-string "<[^>]*>" "")
       (s-replace-all '(("&lt;" . "<") ("&gt;" . ">")
                        ("&amp;" . "&") ("" . "")))))

(defun eopengrok-text-highlight (line)
  (let ((pos 0))
    (while (string-match (concat "\\b" eopengrok-search-text "\\b") line pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'face 'eopengrok-highlight-face line))))

(defun eopengrok-print-line (arg-list)
  (with-current-buffer eopengrok-buffer
    (-if-let* (((file info src) arg-list)
               (file (propertize file 'face 'eopengrok-file-face))
               (info (propertize info 'face 'eopengrok-info-face))
               (src (propertize src 'face 'eopengrok-source-face)))
        (progn
          (unless (string= file eopengrok-last-filename)
            (insert (format "\n%s\n" file))
            (eopengrok-abbreviate-file file)
            (unless eopengrok-first-match-point
              (setq eopengrok-first-match-point (point))))
          (eopengrok-properties-region
           (list :name (expand-file-name file)
                 :info (cond
                        ((equal info "") 1)
                        ((eopengrok-number-p info)
                         (string-to-number info))
                        (t info)))
           (eopengrok-text-highlight src)
           (insert (concat (format "%08s" info) " " src)))
          (insert "\n")
          (setq eopengrok-last-filename file))
      (insert (car arg-list) "\n"))))

(defun eopengrok-read-line (line)
  (cond
   ((string-match eopengrok-text-regexp line))
   ((string-match eopengrok-file-regexp line))
   ((string-match eopengrok-history-regexp line))
   (t (string-match "\\(.*\\)" line)))
  (mapcar (lambda (arg) (match-string arg line)) '(1 2 3)))

(defun eopengrok-process-filter (process output)
  (with-current-buffer eopengrok-buffer
    (let ((buffer-read-only nil)
          (pos 0)
          (output (concat eopengrok-pending-output output)))
      (save-excursion
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (goto-char (point-max))
            (-> line
                eopengrok-remove-html-tags
                eopengrok-read-line
                eopengrok-print-line))))
      (setq eopengrok-pending-output (substring output pos)))))

(defun eopengrok-process-sentinel (process event)
  (with-current-buffer eopengrok-buffer
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (format "\nSearch complete.  Search time = %.2f seconds.\n"
                      (float-time (time-subtract (current-time)
                                                 eopengrok-start-time)))))
    (when eopengrok-first-match-point
      (goto-char eopengrok-first-match-point))))

(defun eopengrok-init (text dir)
  (setq eopengrok-start-time (current-time))
  (setq eopengrok-last-filename nil)
  (setq eopengrok-first-match-point nil)
  (with-current-buffer eopengrok-buffer
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (s-repeat 80 "="))
      (insert (format "\nFinding text string: %s" text))
      (insert (format "\nDirectory: %s\n"
                      (s-chop-suffix eopengrok-database dir))))))

(defmacro eopengrok-define-find (sym option)
  "Define function."
  (let ((fun (intern (format "eopengrok-find-%s" sym)))
        (doc (format "Find option %s" option))
        (str (format "Find %s: " sym)))
    `(progn
       (defun ,fun (text) ,doc
              (interactive (list (read-string ,str (current-word))))
              (let* ((dir (eopengrok-search-configuration))
                     (proc (apply 'start-process
                                  "eopengrok"
                                  eopengrok-buffer
                                  "java"
                                  (eopengrok-search-option-list
                                   dir ,option text))))
                (eopengrok-init text dir)
                (setq eopengrok-search-text text)
                (set-process-filter proc 'eopengrok-process-filter)
                (set-process-sentinel proc 'eopengrok-process-sentinel)
                (with-current-buffer eopengrok-buffer
                  (eopengrok-mode t)
                  (setq buffer-read-only t)
                  (set-buffer-modified-p nil)
                  (pop-to-buffer eopengrok-buffer)
                  (goto-char (point-max))))))))

(eopengrok-define-find definition "-d")
(eopengrok-define-find file "-p")
(eopengrok-define-find reference "-r")
(eopengrok-define-find text "-f")
(eopengrok-define-find history "-h")

(defun eopengrok-index-files (dir)
  "Index files in a directory."
  (interactive "DIndex files in directory: ")
  (apply 'start-process
         "eopengrok-indexer"
         eopengrok-indexing-buffer
         "java"
         (eopengrok-index-option-list (expand-file-name dir)))
  (with-current-buffer eopengrok-indexing-buffer
    (pop-to-buffer eopengrok-indexing-buffer)
    (goto-char (point-max))))

(defvar eopengrok-mode-map nil
  "Keymap for eopengrok minor mode.")

(unless eopengrok-mode-map
  (setq eopengrok-mode-map (make-sparse-keymap)))

(--each '(("\C-csI"   . eopengrok-index-files)
          ("\C-csd"   . eopengrok-find-definition)
          ("\C-csf"   . eopengrok-find-file)
          ("\C-css"   . eopengrok-find-reference)
          ("\C-cst"   . eopengrok-find-text)
          ("\C-csh"   . eopengrok-find-history)
          ("\C-csl"   . eopengrok-switch-to-buffer)
          ("n"        . eopengrok-next-line)
          ("p"        . eopengrok-previous-line)
          ("y"        . eopengrok-say-yes)
          ("<return>" . eopengrok-jump-to-source))
  (define-key eopengrok-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode eopengrok-mode
  "Minor mode for opengrok."
  nil " eopengrok" eopengrok-mode-map)

(provide 'eopengrok)
;;; eopengrok.el ends here
