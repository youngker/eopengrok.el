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
(require 'cl-lib)

(defvar eopengrok-pending-output nil)
(defvar eopengrok-last-filename nil)
(defvar eopengrok-search-text nil)
(defvar eopengrok-first-match-point nil)
(defvar eopengrok-time nil)

(defconst eopengrok-buffer "*eopengrok*")
(defconst eopengrok-indexing-buffer "*eopengrok-indexing-buffer*")

(defcustom eopengrok-jar
  "/home/youngjooez.lee/Projects/opengrok-0.12.1/lib/opengrok.jar"
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
  '("CVS" "RCS" "SCCS" ".git" ".hg" ".bzr" ".cdv" ".pc" ".svn" "_MTN" "_darcs"
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

(defface eopengrok-number-face
  '((t :inherit font-lock-constant-face))
  "Face for line number."
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
  (-flatten (list "-Xms128m" "-Xmx1024m"
                  "-cp" eopengrok-jar "org.opensolaris.opengrok.index.Indexer"
                  "-c" eopengrok-ctags
                  "-W" (concat dir eopengrok-database)
                  "-d" (concat dir ".opengrok")
                  "-s" dir
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
                    (directory-file-name dir))))))))

(defun eopengrok-search-option-list (dir find-option text)
  (list "-Xms128m" "-Xmx1024m"
        "-cp" eopengrok-jar "org.opensolaris.opengrok.search.Search"
        "-R" dir find-option text))

(defmacro eopengrok-properties-region (props &rest body)
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun eopengrok-get-properties (pos)
  (list (get-text-property pos :file-name)
        (get-text-property pos :file-number)))

(defun eopengrok-preview-source ()
  (with-current-buffer eopengrok-buffer
    (-if-let* (((file number) (eopengrok-get-properties (point))))
        (let* ((buffer (find-file-noselect file))
               (window (display-buffer buffer)))
          (set-buffer buffer)
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- number)))
          (set-window-point window (point))
          window))))

(defun eopengrok-jump-to-source ()
  (interactive)
  (-when-let (window (eopengrok-preview-source))
    (select-window window)
    (ring-insert find-tag-marker-ring (point-marker))))

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
       (s-replace-all '(("&lt;" . "<") ("&gt;" . ">")
                        ("&amp;" . "&") ("" . "")))))

(defun eopengrok-text-highlight (line)
  (let ((pos 0))
    (while (string-match (concat "\\b" eopengrok-search-text "\\b") line pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'face 'eopengrok-highlight-face line))))

(defun eopengrok-make-entry-line (arg-list)
  (-let* (((_ file number line) arg-list)
          (file (propertize (replace-regexp-in-string ":$" "" file)
                            'face 'eopengrok-file-face))
          (number (propertize number
                              'face 'eopengrok-number-face))
          (line (propertize (replace-regexp-in-string "^\\[" "" line)
                            'face 'eopengrok-source-face)))
    (unless (string= file eopengrok-last-filename)
      (insert (format "\n%s\n" file))
      (eopengrok-abbreviate-file file)
      (unless eopengrok-first-match-point
       (setq eopengrok-first-match-point (point))))
    (eopengrok-properties-region
     (list :file-name (expand-file-name file)
           :file-number (string-to-number number))
     (eopengrok-text-highlight line)
     (insert (concat (format "%05s" number) " " line "\n")))
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
  (with-current-buffer eopengrok-buffer
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (format "\nSearch complete.  Search time = %.2f seconds.\n"
                    (float-time (time-subtract (current-time)
                                               eopengrok-time))))
    (goto-char eopengrok-first-match-point)))

(defun eopengrok-init (text dir)
  (with-current-buffer eopengrok-buffer
    (setq eopengrok-time (current-time))
    (setq buffer-read-only nil)
    (setq eopengrok-last-filename nil)
    (setq eopengrok-first-match-point nil)
    (goto-char (point-max))
    (insert (s-repeat 80 "="))
    (insert (format "\nFinding text string: %s" text))
    (insert (format "\nDirectory: %s\n"
                    (s-chop-suffix eopengrok-database dir)))))

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
                  (set-buffer-modified-p nil))
                (pop-to-buffer eopengrok-buffer)
                (goto-char (point-max)))))))

(eopengrok-define-find definition "-d")
(eopengrok-define-find file "-p")
(eopengrok-define-find reference "-r")
(eopengrok-define-find text "-f")

(defun eopengrok-index-files (dir)
  "Index files in a directory."
  (interactive "DIndex files in directory: ")
  (apply 'start-process
         "eopengrok-indexer"
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
