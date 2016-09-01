;;; eopengrok.el --- opengrok interface for emacs

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.4.0
;; Keywords: tools
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (magit "2.1.0") (cl-lib "0.5"))

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

;; opengrok interface for Emacs
;;
;; See documentation on https://github.com/youngker/eopengrok.el

;;; Code:

(require 's)
(require 'dash)
(require 'etags)
(require 'magit)
(require 'cl-lib)

(defvar eopengrok-pending-output nil)
(defvar eopengrok-last-filename nil)
(defvar eopengrok-start-time nil)
(defvar eopengrok-search-text nil)
(defvar eopengrok-current-search nil)
(defvar eopengrok-page-number nil)
(defvar-local eopengrok-mode-line-status 'not-running)

(defconst eopengrok-buffer "*eopengrok*")
(defconst eopengrok-indexing-buffer "*eopengrok-indexing-buffer*")

(defconst eopengrok-source-regexp
  "^\\([^ ]*?\\):\\([0-9]+\\):\\(.*\\)")

(defconst eopengrok-history-regexp
  "^\\([^ ]*?\\)::[ \t]+\\(\\w+\\)\\(.*\\)")

(defgroup eopengrok nil
  "Opengrok interface for emacs."
  :prefix "eopengrok-"
  :group 'applications)

(defcustom eopengrok-configuration
  ".opengrok/configuration.xml"
  "Configuration file."
  :type 'string
  :group 'eopengrok)

(defcustom eopengrok-abbreviate-filename 80
  "Abbreviate filename length."
  :type 'number
  :group 'eopengrok)

(defcustom eopengrok-line-length 500
  "Truncate line length."
  :type 'number
  :group 'eopengrok)

(defcustom eopengrok-mode-line '(:eval (eopengrok-mode-line-page))
  "Mode line lighter for eopengrok."
  :group 'eopengrok
  :type 'sexp
  :risky t)

(defcustom eopengrok-mode-line-prefix "EOG"
  "Mode line prefix."
  :group 'eopengrok
  :type 'string)

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

(defun eopengrok-resume ()
  "Resume *eopengrok* buffer."
  (interactive)
  (when (get-buffer eopengrok-buffer)
    (pop-to-buffer eopengrok-buffer)))

(defun eopengrok-kill-process ()
  "Kill process."
  (interactive)
  (-if-let* ((proc (get-process "eopengrok"))
             (status (process-status proc))
             (run (eq status 'run)))
      (kill-process proc)
    (kill-buffer eopengrok-buffer)))

(defun eopengrok-get-configuration ()
  "Search for Project configuration.xml."
  (let* ((start-dir (expand-file-name default-directory))
         (index-dir (locate-dominating-file start-dir eopengrok-configuration)))
    (if index-dir
        (concat (expand-file-name index-dir) eopengrok-configuration)
      (error "Can't find configuration.xml"))))

(defun eopengrok-search-option-list (configuration find-option text)
  "Opengrok search option list with CONFIGURATION FIND-OPTION TEXT."
  (list "search" "-R" configuration find-option text))

(defun eopengrok-custom-option-list (configuration text)
  "Opengrok search option list with CONFIGURATION FIND-OPTION TEXT."
  (-flatten (list "search" "-R" configuration (split-string text " " t))))

(defmacro eopengrok-properties-region (props &rest body)
  "Add PROPS and Execute BODY to all the text it insert."
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun eopengrok-get-properties (pos)
  "Get properties at POS."
  (list (get-text-property pos :name)
        (get-text-property pos :info)))

(defun eopengrok-show-source ()
  "Display original source."
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
  "Display magit-show-commit."
  (-when-let* (((file commit-id) (eopengrok-get-properties (point))))
    (setq default-directory (file-name-directory file))
    (magit-git-string "rev-parse" "--show-toplevel")
    (magit-show-commit commit-id)))

(defun eopengrok-jump-to-source ()
  "Jump point to the other window."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (-when-let (info (get-text-property (point) :info))
      (if (numberp info)
          (-when-let (window (eopengrok-show-source))
            (select-window window)
            (ring-insert find-tag-marker-ring (point-marker)))
        (eopengrok-show-commit)))))

(defun eopengrok-number-p (str)
  "Check commitid from STR."
  (< (length str) 8))

(defun eopengrok-next-line ()
  "Move point to the next search result, if one exists."
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (next-single-property-change
                     (save-excursion (end-of-line) (point)) :info))
      (goto-char pos)
      (if (numberp (get-text-property pos :info))
          (eopengrok-show-source)
        (let ((magit-display-buffer-noselect t))
          (eopengrok-show-commit))))))

(defun eopengrok-previous-line ()
  "Move point to the previous search result, if one exists."
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (previous-single-property-change
                     (save-excursion (beginning-of-line) (point)) :info))
      (goto-char pos)
      (beginning-of-line)
      (if (numberp (get-text-property (point) :info))
          (eopengrok-show-source)
        (let ((magit-display-buffer-noselect t))
          (eopengrok-show-commit))))))

(defun eopengrok-abbreviate-file (file)
  "Abbreviate FILE name."
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
      (cl-decf amount (funcall advance-word)))
    (goto-char end)))

(defun eopengrok-truncate-line (line)
  "Truncate long text from LINE."
  (let ((len eopengrok-line-length))
    (if (> (length line) len)
        (format "%s...]" (substring line 0 (- len 4)))
      line)))

(defun eopengrok-remove-html-tags (line)
  "Remove html tag from LINE."
  (->> line
       (replace-regexp-in-string "<[^>]*>" "")
       (s-replace-all '(("&lt;" . "<") ("&gt;" . ">")
                        ("&amp;" . "&") ("\r" . "")))))

(defun eopengrok-text-highlight (line)
  "Highlighting Text from LINE."
  (let (beg end)
    (with-temp-buffer
      (insert (propertize line 'read-only nil))
      (goto-char (point-min))
      (while (and (re-search-forward
                   (s-replace "\"" "" eopengrok-search-text) nil t)
                  (> (- (setq end (match-end 0))
                        (setq beg (match-beginning 0))) 0))
        (add-text-properties beg end '(face eopengrok-highlight-face)))
      (buffer-string))))

(defun eopengrok-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (eopengrok-jump-to-source))

(defvar eopengrok-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'eopengrok-handle-mouse)
    map))

(defun eopengrok-print-line (arg-list)
  "Print line from ARG-LIST with their propertize."
  (with-current-buffer eopengrok-buffer
    (-if-let* (((file info src) arg-list)
               (file (propertize file 'face 'eopengrok-file-face))
               (info (propertize info
                                 'face 'eopengrok-info-face
                                 'mouse-face 'highlight
                                 'keymap eopengrok-mouse-map))
               (src (propertize src 'face 'eopengrok-source-face)))
        (eopengrok-properties-region
         (list :page eopengrok-page-number)
         (progn
           (unless (string= file eopengrok-last-filename)
             (insert (format "\n%s\n" file))
             (eopengrok-abbreviate-file file))
           (eopengrok-properties-region
            (list :name (expand-file-name file)
                  :info (cond
                         ((equal info "") 1)
                         ((eopengrok-number-p info)
                          (string-to-number info))
                         (t info)))
            (insert (concat (format "%08s" info) " "
                            (eopengrok-text-highlight src))))
           (insert "\n")
           (setq eopengrok-last-filename file)))
      (-if-let* (((_ cur-page total-page)
                  (s-match "clj-opengrok> \\([0-9]+\\)/\\([0-9]+\\)"
                           (car arg-list))))

          (progn
            (setq eopengrok-mode-line-status 'running)
            (setq eopengrok-page-number (format "%s/%s" cur-page total-page)))
        (insert (car arg-list) "\n")))))

(defun eopengrok-read-line (line)
  "Read the LINE and return the list for print."
  (cond
   ((string-match eopengrok-source-regexp line))
   ((string-match eopengrok-history-regexp line))
   (t (string-match "\\(.*\\)" line)))
  (mapcar (lambda (arg) (match-string arg line)) '(1 2 3)))

(defun eopengrok-process-filter (process output)
  "Process eopengrok output from PROCESS containted in OUTPUT."
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
  "Handle eopengrok PROCESS EVENT."
  (with-current-buffer eopengrok-buffer
    (cond ((string-match "killed" event)
           (kill-buffer eopengrok-buffer))
          ((string-match "finished" event)
           (setq eopengrok-mode-line-status 'finished)))))

(defun eopengrok-display-info (configuration)
  "Print information with CONFIGURATION."
  (with-current-buffer eopengrok-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert (format "%s%s" eopengrok-current-search
                      eopengrok-search-text))
      (insert (format "\nDirectory: %s\n"
                      (s-chop-suffix eopengrok-configuration
                                     configuration)))
      (forward-line -2))))

(defun eopengrok-init (text configuration kind)
  "Initialize function from TEXT & CONFIGURATION & KIND."
  (setq eopengrok-search-text text)
  (setq eopengrok-last-filename nil)
  (setq eopengrok-current-search kind)
  (setq eopengrok-pending-output nil)
  (setq eopengrok-page-number nil)
  (eopengrok-display-info configuration))

(defmacro eopengrok-define-find (sym option)
  "Make function with SYM and OPTION."
  (let ((fun (intern (format "eopengrok-find-%s" sym)))
        (doc (format "Find option %s" option))
        (str (format "Find %s: " sym)))
    `(progn
       (defun ,fun (text) ,doc
              (interactive (list (read-string ,str (current-word))))
              (let ((conf (eopengrok-get-configuration))
                    (proc (get-process "eopengrok")))
                (print ',sym)
                (when proc
                  (kill-process proc)
                  (sleep-for 0.1))
                (let ((proc (apply 'start-process
                                   "eopengrok"
                                   eopengrok-buffer
                                   "clj-opengrok"
                                   (if (eq ',sym 'custom)
                                       (eopengrok-custom-option-list conf text)
                                     (eopengrok-search-option-list
                                      conf ,option text)))))
                  (eopengrok-init text conf ,str)
                  (set-process-filter proc 'eopengrok-process-filter)
                  (set-process-sentinel proc 'eopengrok-process-sentinel)))
              (with-current-buffer eopengrok-buffer
                (eopengrok-mode t)
                (setq truncate-lines t)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (pop-to-buffer eopengrok-buffer))))))

(eopengrok-define-find definition "-d")
(eopengrok-define-find file "-p")
(eopengrok-define-find reference "-r")
(eopengrok-define-find text "-f")
(eopengrok-define-find history "-h")
(eopengrok-define-find custom "")

(defun eopengrok-index-process-sentinel (process event)
  "Handle eopengrok PROCESS EVENT."
  (with-current-buffer eopengrok-indexing-buffer
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (format "\nIndexing complete.  Indexing time = %.2f seconds.\n"
                      (float-time (time-subtract (current-time)
                                                 eopengrok-start-time)))))))

(defun eopengrok-make-index (dir &optional enable-projects-p)
  "Make an Index file in DIR, ENABLE-PROJECTS-P is flag for enable projects.
If not nil every directory in DIR is considered a separate project."
  (interactive "DRoot directory: ")
  (let ((proc (apply 'start-process
                     "eopengrok-indexer"
                     eopengrok-indexing-buffer
                     "clj-opengrok"
                     (remove nil (list "index" "-s" (expand-file-name dir)
                                       (when enable-projects-p "-e"))))))
    (set-process-sentinel proc 'eopengrok-index-process-sentinel)
    (with-current-buffer eopengrok-indexing-buffer
      (setq buffer-read-only t)
      (setq eopengrok-start-time (current-time))
      (goto-char (point-max))
      (pop-to-buffer eopengrok-indexing-buffer))))

(defun eopengrok-make-index-with-enable-projects (dir)
  "Make an Index file, every directory in DIR is considered a separate project."
  (interactive "DRoot directory (enable projects): ")
  (eopengrok-make-index dir t))

(defvar eopengrok-mode-map nil
  "Keymap for eopengrok minor mode.")

(unless eopengrok-mode-map
  (setq eopengrok-mode-map (make-sparse-keymap)))

(--each '(("n"        . eopengrok-next-line)
          ("p"        . eopengrok-previous-line)
          ("q"        . eopengrok-kill-process)
          ("<return>" . eopengrok-jump-to-source))
  (define-key eopengrok-mode-map (read-kbd-macro (car it)) (cdr it)))

(defun eopengrok-mode-line-page ()
  "Get a page in the mode line."
  (let ((page (or (get-text-property (point) :page)
                  eopengrok-page-number))
        (status (pcase eopengrok-mode-line-status
                  (`running "*:")
                  (`finished ":")
                  (`not-running "-"))))
    (concat " " eopengrok-mode-line-prefix status page)))

(define-minor-mode eopengrok-mode
  "Minor mode for opengrok."
  :lighter eopengrok-mode-line
  nil " eopengrok" eopengrok-mode-map)

(provide 'eopengrok)

;;; eopengrok.el ends here
