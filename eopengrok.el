;;; eopengrok.el --- opengrok interface for emacs

;; Copyright (C) 2015 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

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

(defvar eopengrok-pending-output nil)
(defvar eopengrok-last-filename nil)

(defconst eopengrok-buffer "*eopengrok*")
(defconst eopengrok-indexing-buffer "*eopengrok-indexing-buffer*")

(defcustom eopengrok-jar
  "/Users/youngker/Projects/opengrok-0.12.1.5/lib/opengrok.jar"
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
  '("-I" "*.c" "-I" "*.h" "-I" "*.cpp" "-I" "*.hpp" "-I" "*.cc" "-I" "*.hh"
    "-I" "*.m" "-I" "*.xml" "-I" "*.mk" "-I" "*.py" "-I" "*.rb" "-I" "*.java"
    "-I" "*.js" "-I" "*.el")
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-ignored-dir
  '("-i" "CVS" "-i" "RCS" "-i" "SCCS" "-i" ".git" "-i" ".hg" "-i" ".bzr"
    "-i" ".cdv" "-i" ".pc" "-i" ".svn" "-i" "_MTN" "-i" "_darcs" "-i" "_sgbak"
    "-i" "debian" "-i" ".opengrok" "-i" "out" "-i" ".repo")
  "DOC."
  :group 'eopengrok)

(defcustom eopengrok-abbreviate-filename 80
  "DOC."
  :group 'eopengrok)

(defface eopengrok-file-face
  '((((class color) (background dark))
     (:foreground "LightSkyBlue"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face for files."
  :group 'eopengrok)

(defface eopengrok-number-face
  '((((class color) (background dark))
     (:foreground "SeaGreen2"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face for line number."
  :group 'eopengrok)

(defface eopengrok-source-face
  '((((class color) (background dark))
     (:foreground "darkorange3"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face for source."
  :group 'eopengrok)

(defun eopengrok-index-option-list (dir)
  (-flatten (list "-Xms128m" "-Xmx1024m"
                  "-cp" eopengrok-jar "org.opensolaris.opengrok.index.Indexer"
                  "-c" eopengrok-ctags
                  "-W" (concat dir eopengrok-database)
                  "-d" (concat dir ".opengrok")
                  "-s" dir
                  eopengrok-indexer-suffixes
                  eopengrok-ignored-dir)))

(defun eopengrok-search-configuration ()
  (let ((dir (expand-file-name default-directory)))
    (catch 'done
      (while dir
        (when (file-exists-p (concat dir eopengrok-database))
          (throw 'done (concat dir eopengrok-database)))
        (setq dir (file-name-as-directory
                   (file-name-directory
                    (directory-file-name dir))))))))

(defun eopengrok-search-option-list (find-option text)
  (list "-Xms128m" "-Xmx1024m"
        "-cp" eopengrok-jar "org.opensolaris.opengrok.search.Search"
        "-R" (eopengrok-search-configuration)
        find-option text))

(defmacro eopengrok-properties-region (props &rest body)
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun eopengrok-get-properties ()
  (list (get-text-property (point) :file-name)
        (get-text-property (point) :file-number)))

(defun eopengrok-preview-source ()
  (with-current-buffer eopengrok-buffer
    (-let (((file number) (eopengrok-get-properties)))
      (setq buffer (find-file-noselect file))
      (setq window (display-buffer buffer))
      (set-buffer buffer)
      (goto-line number)
      (set-window-point window (point))))
  window)

(defun eopengrok-jump-to-source ()
  (interactive)
  (select-window (eopengrok-preview-source))
  (ring-insert find-tag-marker-ring (point-marker)))

(defun eopengrok-next-line ()
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (next-single-property-change (point) :file-number))
      (while (not (get-text-property pos :file-number))
        (setq pos (next-single-property-change pos :file-number)))
      (goto-char pos)
      (eopengrok-preview-source))))

(defun eopengrok-previous-line ()
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (previous-single-property-change (point) :file-number))
      (while (not (get-text-property pos :file-number))
        (setq pos (previous-single-property-change pos :file-number)))
      (goto-char pos)
      (eopengrok-preview-source))))

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
       (replace-regexp-in-string "]$" "")
       (s-replace-all '(("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&") ("" . "")))))

(defun eopengrok-make-entry-line (arg-list)
  (-let (((_ file number line) arg-list))
    (setq file (replace-regexp-in-string ":$" "" file))
    (setq line (replace-regexp-in-string "^\\[" "" line))
    (unless (string= file eopengrok-last-filename)
      (newline)
      (insert (format "%s:\n"
                      (propertize file 'face 'eopengrok-file-face)))
      (eopengrok-abbreviate-file file))
    (eopengrok-properties-region
     (list :file-name (expand-file-name file)
           :file-number (string-to-number number))
     (insert (concat
              (propertize number 'face 'eopengrok-number-face) ": "
              (propertize line 'face 'eopengrok-source-face))))
    (newline)
    (setq eopengrok-last-filename file)))

(defun eopengrok-read-line (line)
  (-if-let (arg-list (s-match "\\(^/.*:\\)\\([0-9]+\\)[ \t]+\\(.*\\)" line))
      (eopengrok-make-entry-line arg-list)
    (-if-let (arg-list (s-match "\\(^/.*:\\)[ \t]+\\(.*\\)" line))
        (eopengrok-make-entry-line (-insert-at 2 "1" arg-list))
      (insert line "\n"))))

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
                eopengrok-read-line))))
      (setq eopengrok-pending-output (substring output pos)))))

(defun eopengrok-process-sentinel (process event)
  (message (format "eopengrok %S" event)))

(defmacro eopengrok-define-find (sym option)
  "Define function."
  (let ((fun (intern (format "eopengrok-find-%s" sym)))
        (doc (format "Find option %s" option))
        (str (format "Find %s: " sym)))
    `(progn
       (defun ,fun (text) ,doc
              (interactive (list (read-string ,str (current-word))))
              (setq eopengrok-process
                    (apply 'start-process
                           eopengrok-buffer
                           eopengrok-buffer
                           "java"
                           (eopengrok-search-option-list ,option text)))
              (set-process-filter eopengrok-process 'eopengrok-process-filter)
              (set-process-sentinel eopengrok-process 'eopengrok-process-sentinel)
              (with-current-buffer eopengrok-buffer
                (eopengrok-mode t)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil))
              (pop-to-buffer eopengrok-buffer)
              (goto-char (point-max))))))

(eopengrok-define-find definition "-d")
(eopengrok-define-find file "-p")
(eopengrok-define-find reference "-r")
(eopengrok-define-find text "-f")

(defun eopengrok-index-files (dir)
  "Index files in a directory"
  (interactive "DIndex files in directory: ")
  (apply 'start-process
         eopengrok-indexing-buffer
         eopengrok-indexing-buffer
         "java"
         (eopengrok-index-option-list (expand-file-name dir)))
  (with-current-buffer eopengrok-indexing-buffer)
  (pop-to-buffer eopengrok-indexing-buffer)
  (goto-char (point-max)))

(defvar eopengrok-mode-map nil
  "Keymap for eopengrok minor mode.")

(unless eopengrok-mode-map
  (setq eopengrok-mode-map (make-sparse-keymap)))

(--each '(("\C-csI"   . eopengrok-index-files)
          ("\C-csd"   . eopengrok-find-definition)
          ("\C-csf"   . eopengrok-find-file)
          ("\C-css"   . eopengrok-find-reference)
          ("\C-cst"   . eopengrok-find-text)
          ("n"        . eopengrok-next-line)
          ("p"        . eopengrok-previous-line)
          ("<return>" . eopengrok-jump-to-source))
  (define-key eopengrok-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode eopengrok-mode
  "Minor mode for opengrok."
  nil " eopengrok" eopengrok-mode-map)

(provide 'eopengrok)
;;; eopengrok.el ends here
