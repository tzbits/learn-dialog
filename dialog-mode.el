;;; dialog-mode.el --- Major mode for editing Dialog files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  tzbits.com

;; Author: tzbits
;; Keywords: languages, interactive fiction
;; URL: https://github.com/tzbits/learn-dialog/blob/main/dialog-mode.el
;; Package-Requires: ((emacs "29.3"))
;; Version: 0.1.0

;;; Commentary:

;; This package provides a major mode for the Dialog interactive fiction
;; programming language. It includes syntax highlighting, indentation,
;; and paragraph navigation.

;; Installation:
;;
;; To install manually, place dialog-mode.el in your load-path and add
;; the following to your init file:
;;
;;   (require 'dialog-mode)

;; Keybindings:
;;
;; The mode enables indent-tabs-mode and provides context-aware
;; navigation and editing:
;;
;;   M-q   dialog-fill-paragraph  Fill prose respecting Dialog indentation.
;;   M-.   xref-find-definitions  Jump to rule or object definitions.
;;   M-,   xref-go-back           Return to previous location.
;;   M-g i imenu                  Search for objects or section headers.
;;
;; Navigation (Imenu):
;;
;; This mode automatically populates a "Story" menu in the menu bar
;; when using a graphical display. You can use M-g i to quickly
;; navigate to object definitions (#object) or section headers (lines
;; that start with %% in column 0, folowed by a space and a capital
;; letter).

;;; Code:

(require 'cl-lib)
(require 'grep)
(require 'rx)
(require 'xref)

(defgroup dialog nil
  "Support for the Dialog programming language."
  :group 'languages
  :prefix "dialog-")

(rx-define dialog-identifier
  (1+ (or (syntax word) (any "-_+"))))

(defcustom dialog-tab-width 4
  "Default tab width for Dialog files."
  :type 'integer
  :group 'dialog)

(defcustom dialog-fill-column 79
  "Default fill column for Dialog prose."
  :type 'integer
  :group 'dialog)

(defcustom dialog-ignored-directories '("out")
  "List of directory names to ignore when searching for definitions."
  :type '(repeat string)
  :group 'dialog)

(defvar-keymap dialog-mode-map
  :doc "Keymap for `dialog-mode'."
  "M-q" #'dialog-fill-paragraph)

(defconst dialog-font-lock-keywords
  (list
   ;; These sigils designate global objects, keywords, and variables.
   ;; We match the prefix to ensure they are visually distinct.
   (list (rx "#" dialog-identifier) 0 'font-lock-constant-face)
   (list (rx "@" dialog-identifier) 0 'font-lock-keyword-face)
   (list (rx "$" dialog-identifier) 0 'font-lock-variable-name-face)

   ;; Highlighting only group 1 prevents the parenthesis from being
   ;; colored as part of the function name.
   (list (rx "(" (group-n 1 dialog-identifier)) 1 'font-lock-function-name-face))
  "Keyword highlighting specification for `dialog-mode'.")

(defvar dialog-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; The '12b' flags specify that '%' is the first and second
    ;; character of a two-char comment starter (%%), compatible
    ;; with the 'b' comment style.
    (modify-syntax-entry ?% ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Defining pairs allows Emacs to handle structural movement
    ;; like `forward-sexp` correctly.
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "()" table)
    (modify-syntax-entry ?\] ")(" table)
    (modify-syntax-entry ?\{ "()" table)
    (modify-syntax-entry ?\} ")(" table)

    ;; By marking #, $, and @ as symbol constituents (_), Emacs commands
    ;; like `symbol-at-point` will include these sigils automatically.
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)

    ;; Dialog identifiers frequently use hyphens and underscores.
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for `dialog-mode'.")

;;; Xref Backend Implementation

(defun dialog--project-files ()
  "Return a list of .dg files in the current project or directory."
  (let* ((root (or (vc-root-dir) default-directory))
         ;; We generate the regex once to avoid overhead during
         ;; the recursive directory walk.
         (exclude-re (regexp-opt dialog-ignored-directories 'symbols)))
    (directory-files-recursively
     root
     (rx ".dg" eos)
     nil
     (lambda (dir)
       (let ((dir-name (file-name-nondirectory (directory-file-name dir))))
         (not (string-match-p exclude-re dir-name)))))))

(defun dialog-xref-backend ()
  "Return the backend identifier for Dialog mode."
  'dialog-xref)

(cl-defmethod xref-backend-definitions ((_backend (eql dialog-xref)) symbol)
  "Find definitions for SYMBOL at the start of a line and return a
list of `xref-item' structs."
  (let ((search-regexp (dialog--construct-search-regexp symbol))
        (files (dialog--project-files))
        (xref-items nil))
    (when search-regexp
      (dolist (file files)
        (with-temp-buffer
          ;; We use a temp buffer and `insert-file-contents' to avoid the
          ;; overhead of `find-file-noselect' which would trigger major modes
          ;; and hooks for every file in the project.
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward search-regexp nil t)
            (let* ((line (line-number-at-pos))
                   (bol (line-beginning-position))
                   (location (xref-make-file-location file line (- (point) bol (length symbol))))
                   (summary (buffer-substring bol (line-end-position))))
              (push (xref-make summary location) xref-items)))))
      (nreverse xref-items))))

(defun dialog--construct-search-regexp (symbol)
  "Determine the correct search pattern based on the SYMBOL prefix."
  (cond
   ((string-prefix-p "#" symbol)
    ;; Objects are defined as the identifier alone at the start of a line.
    (concat "^" (regexp-quote symbol) "$"))
   ((dialog--at-symbol-with-prefix-p "(")
    ;; Rules are defined starting with ( or *(.
    (concat "^\\(?:(\\|\\*(\\)" (regexp-quote symbol)))
   (t nil)))

(defun dialog--at-symbol-with-prefix-p (prefix)
  "Return non-nil if point is on a symbol preceded by PREFIX string."
  (save-excursion
    (skip-syntax-backward "w_")
    (let ((prefix-len (length prefix)))
      (and (>= (- (point) (point-min)) prefix-len)
           (string= (buffer-substring (- (point) prefix-len) (point))
                    prefix)))))

;;; Paragraph Handling

(defun dialog-fill-paragraph (&optional justify)
  "Fill the current paragraph.
JUSTIFY is passed to `fill-region-as-paragraph'."
  (interactive "P")
  (save-excursion
    (let ((start (progn (dialog-beginning-of-paragraph) (point)))
          (end (progn (dialog-end-of-paragraph) (point))))
      (fill-region-as-paragraph start end justify))))

(defun dialog-beginning-of-paragraph ()
  "Move point to the beginning of the current Dialog paragraph."
  (interactive)
  (back-to-indentation)
  (let ((col (current-column)))
    (catch 'done
      (when (dialog--looking-at-special-p)
        (throw 'done t))
      (while (and (not (bobp))
                  (= (dialog--get-previous-indent)
                     col))
        (forward-line -1)
        (back-to-indentation)
        (when (looking-at-p "(par)")
          (throw 'done t))
        (when (dialog--looking-at-special-p)
          (forward-line 1)
          (back-to-indentation)
          (throw 'done t))))))

(defun dialog-end-of-paragraph ()
  "Move point to the end of the current Dialog paragraph."
  (interactive)
  (back-to-indentation)
  (let ((col (current-column)))
    (catch 'done
      (while (and (not (eobp))
                  (= (dialog--get-next-indent) col))
        (forward-line 1)
        (back-to-indentation)
        (when (dialog--looking-at-special-p)
          (forward-line -1)
          (end-of-line)
          (throw 'done t)))))
  (end-of-line))

(defun dialog--looking-at-special-p ()
  "Check if point is at a Dialog special character sequence."
  (looking-at-p (rx (or (any "#$@~*|([{} ") "%%"))))

(defun dialog--get-previous-indent ()
  "Get the indentation of the previous line."
  (save-excursion
    (if (zerop (forward-line -1))
        (progn (back-to-indentation) (current-column))
      -1)))

(defun dialog--get-next-indent ()
  "Get the indentation of the next line."
  (save-excursion
    (if (zerop (forward-line 1))
        (progn (back-to-indentation) (current-column))
      -1)))

;;;###autoload
(define-derived-mode dialog-mode text-mode "Dialog"
  "Major mode for editing Dialog interactive fiction files.

\\{dialog-mode-map}"
  :syntax-table dialog-mode-syntax-table

  (setq-local indent-tabs-mode t)
  (setq-local tab-width dialog-tab-width)
  (setq-local fill-column dialog-fill-column)
  (setq-local comment-start "%% ")
  (setq-local font-lock-defaults '(dialog-font-lock-keywords))

  (setq-local imenu-generic-expression
              `((nil "^\\s-*\\(#" dialog-identifier "\\)" 1)
                (nil "^%% \\([A-Z].*[^\n]\\)" 1)))

  ;; Calling this ensures that Imenu is populated and accessible
  ;; via the menu bar in GUI sessions.
  (imenu-add-to-menubar "Story")

  (setq-local fill-paragraph-function #'dialog-fill-paragraph)

  ;; Add hooks with DEPTH `nil' and LOCAL `t' to ensure they only
  ;; trigger in Dialog buffers.
  (add-hook 'xref-backend-functions #'dialog-xref-backend nil t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dg\\'" . dialog-mode))

(provide 'dialog-mode)

;;; dialog-mode.el ends here
