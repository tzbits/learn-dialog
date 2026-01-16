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
;;
;; Installation:
;;
;; To install manually, place dialog-mode.el in your load-path and add
;; the following to your init file:
;;
;;   (require 'dialog-mode)
;;
;; Keybindings:
;;
;; The mode enables indent-tabs-mode and provides context-aware
;; navigation and editing:
;;
;;   M-q   dialog-fill-paragraph  Fill prose respecting Dialog indentation.
;;   M-.   xref-find-definitions  Jump to rule or object definitions.
;;   M-,   xref-go-back           Return to previous location.
;;
;; Commenting (emacs default bindings):
;;
;;   M-;     comment-dwim           Inserts %% before or after lines
;;   C-c C-; dialog-insert-divider  Inserts % divider, dialog-fill-column wide
;;
;; Navigation (Imenu):
;;
;; This mode populates a "Story" menu in the menu bar when using a
;; graphical display. You can also use M-g i to navigate to object
;; definitions (#object) or section headers (lines that start with %%
;; in column 0, folowed by a space and a capital letter).

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
  "M-q" 'dialog-fill-paragraph
  "C-c C-;" 'dialog-insert-comment-divider
  "C-c C-f" 'dialog-format-buffer)

(defconst dialog-font-lock-keywords
  (list
   (list (rx "#" dialog-identifier) 0 'font-lock-constant-face)
   (list (rx "@" dialog-identifier) 0 'font-lock-keyword-face)
   (list (rx "$" dialog-identifier) 0 'font-lock-variable-name-face)

   ;; Highlight only group 1 to prevent the parenthesis from being
   ;; colored as part of the function name.
   (list (rx "(" (group-n 1 dialog-identifier)) 1 'font-lock-function-name-face))
  "Keyword highlighting specification for `dialog-mode'.")

(defvar dialog-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Use '12b' flags to specify that '%' is a two-char comment
    ;; starter (%%), using the 'b' comment style.
    (modify-syntax-entry ?% ". 12b" table)
    ;; Declare newline as the end delimiter of the 'b' comment style.
    (modify-syntax-entry ?\n "> b" table)

    ;; Define pairs to handle structural movement correctly.
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "()" table)
    (modify-syntax-entry ?\] ")(" table)
    (modify-syntax-entry ?\{ "()" table)
    (modify-syntax-entry ?\} ")(" table)

    ;; Marking #, $, and @ as symbol constituents (_), so that Emacs
    ;; commands like `symbol-at-point` will include these sigils.
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)

    ;; Dialog identifiers may use hyphens and underscores.
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?_ "_" table)

    ;; There are no bracketed strings in dialog.
    (modify-syntax-entry ?\" "w" table)

    table)
  "Syntax table for `dialog-mode'.")

;;; Xref Backend Implementation

(defun dialog--project-files ()
  "Return a list of .dg files in the current project or directory."
  (let* ((root (or (vc-root-dir) default-directory))
         ;; Generate the regex once to avoid overhead during the
         ;; recursive directory walk.
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
          ;; Use a temp buffer and `insert-file-contents' to avoid the
          ;; overhead of `find-file-noselect', which would trigger
          ;; major modes and hooks for every file in the project.
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
    ;; Dialog objects are declared by placing the identifier alone at
    ;; the start of a line.
    (concat "^" (regexp-quote symbol) "$"))
   ((dialog--at-symbol-with-prefix-p "(")
    ;; Rules are defined by lines starting with ( or *(.
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

(defun dialog--looking-at-special-p ()
  "Check if point is at a Dialog special character sequence."
  (looking-at-p (rx (any "#$@~*|([{} "))))

(defun dialog--looking-at-comment-p ()
  (looking-at-p "%%"))

(defun dialog--looking-at-css-rule-p ()
  (looking-at "\\([a-z-]+\\):[ \t]*\\([^;\n]+\\);"))

(defun dialog--looking-at-rule-logic-p ()
  "Check if this is rule logic (not an inline rule call followed by story text)."
  (and (dialog--looking-at-special-p)
       (or (not (looking-at "("))
           (not
            ;; an embedded rule like: (par) The dog sat.
            (save-excursion
              (let ((line (line-number-at-pos)))
                (forward-sexp 1)
                (skip-chars-forward " \t")
                (and (= line (line-number-at-pos))
                     (not (dialog--looking-at-special-p))
                     (not (eolp)))))))))

(defun dialog-fill-paragraph (&optional justify)
  "Fill the current paragraph.
JUSTIFY is passed to `fill-region-as-paragraph'."
  (interactive "P")
  (cond
   ((save-excursion (back-to-indentation) (looking-at-p "\\s-*$")))
   ((dialog--looking-at-compact-rules-p) (dialog-align-compact-rules))
   (t
    (save-excursion
      (let ((start (progn (dialog-beginning-of-paragraph) (point)))
            (end (progn (dialog-end-of-paragraph) (point))))
        (when (/= (line-number-at-pos start) (line-number-at-pos end))
          (if (save-excursion (goto-char start) (dialog--looking-at-comment-p))
              (fill-region start end justify)
            (fill-region-as-paragraph start end justify))))))))

(defun dialog-beginning-of-paragraph ()
  "Move point to the beginning of the current Dialog paragraph."
  (interactive)
  (back-to-indentation)
  (let ((col (current-column)))
    (catch 'done

      (when (dialog--looking-at-comment-p)
        (dialog--beginning-of-comment)
        (throw 'done t))

      (when (dialog--looking-at-css-rule-p)
        (throw 'done t))

      (when (dialog--looking-at-rule-logic-p)
        (throw 'done t))

      (while (and (not (bobp))
                  (= (dialog--get-previous-indent) col))
        (forward-line -1)
        (back-to-indentation)
        (when (looking-at-p "(par)")
          (throw 'done t))
        (when (dialog--looking-at-rule-logic-p)
          (forward-line 1)
          (back-to-indentation)
          (throw 'done t))))))

(defun dialog-end-of-paragraph ()
  "Move point to the end of the current Dialog paragraph."
  (interactive)
  (back-to-indentation)
  (let ((col (current-column)))
    (catch 'done

      (when (dialog--looking-at-comment-p)
        (dialog--end-of-comment)
        (throw 'done t))

      (when (dialog--looking-at-css-rule-p)
        (end-of-line)
        (throw 'done t))

      (when (looking-at-p "\\s-*$")
        (throw 'done t))

      (when (dialog--looking-at-rule-logic-p)
        (end-of-line)
        (throw 'done t))

      (while (and (not (eobp))
                  (= (dialog--get-next-indent) col)
                  (not (dialog--looking-at-rule-logic-p))
                  (not (save-excursion
                         (forward-line 1)
                         (back-to-indentation)
                         (looking-at-p "\\s-*$"))))
        (forward-line 1)
        (back-to-indentation)
        (when (dialog--looking-at-rule-logic-p)
          (forward-line -1)
          (end-of-line)
          (throw 'done t)))))
  (end-of-line))

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

;;; Compact Rules

(defun dialog--looking-at-compact-rules-p ()
  "Return non-nil if point is in a block of compact rules."
  (save-excursion
    (forward-line 0)
    ;; We check the current, previous, or next line because the user
    ;; might trigger fill-paragraph while the cursor is anywhere in the block.
    (looking-at-p "^(")))

(defun dialog-align-compact-rules ()
  "Align the rule bodies in a compact block."
  (interactive)
  (save-excursion
    (let ((max-head-column 0)
          (head-end-columns nil))

      ;; Point may be anywhere in the block, so move to beginning of line
      ;; and navigate to the start of the compact block of rules.
      (forward-line 0)
      (while (and (not (bobp))
                  (save-excursion (forward-line -1) (looking-at-p "^(")))
        (forward-line -1))

      ;; Rule heads are different lengths, so find the column where
      ;; the longest one ends, while also collecting the end columns
      ;; of all the rule heads that need to be formatted, so they can
      ;; be formatted without re-searching.
      (save-excursion
        (while (and (not (eobp)) (looking-at-p "^("))
          (forward-sexp 1 nil)
          (when (not (looking-at-p "\\s-*$"))
            (setq max-head-column (max max-head-column (current-column))))
          (push (point) head-end-columns)
          (forward-line 1)))

      (let ((align-column (if (< (+ max-head-column 1) (* 4 dialog-tab-width))
                              (* 4 dialog-tab-width)
                            (align-to-tab-stop max-head-column)))
            ;; Shadow indent-tabs-mode to nil so that `indent-to` uses
            ;; spaces for alignment.
            (indent-tabs-mode nil))
        (dolist (col head-end-columns)
          (goto-char col)
          (delete-horizontal-space)
          (unless (eolp)
            (indent-to align-column)))))))

(defun align-to-tab-stop (col)
  (let ((stop (* (/ col dialog-tab-width) dialog-tab-width)))
    (if (<= stop (1+ col))
        (+ stop dialog-tab-width)
      stop)))

;;; Format buffer

(defun dialog-format-buffer ()
  "Iterate through the buffer and fill all paragraphs and compact rules."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (back-to-indentation)
      (cond
       ((dialog--looking-at-comment-p)
        ;; Don't touch comments: they are often specially formatted.
        (dialog--end-of-comment)
        (forward-line 1))

       ((dialog--looking-at-compact-rules-p)
        (dialog-align-compact-rules)
        ;; dialog-end-of-paragraph treats each individual rule as a
        ;; paragraph, but compact rules are all formatted once
        ;; together, so move forward out of the block.
        (while (and (not (eobp)) (dialog--looking-at-compact-rules-p))
          (forward-line 1)))

       ((looking-at-p "^\\s-*$")
        (forward-line 1))

       (t
        (dialog-fill-paragraph)
        (dialog-end-of-paragraph)
        (unless (eobp)
          (forward-line 1)))))))


;;; Commenting

(defun dialog--beginning-of-comment ()
  (while (and (not (bobp))
              (save-excursion
                (forward-line -1)
                (back-to-indentation)
                (dialog--looking-at-comment-p)))
    (forward-line -1)))

(defun dialog--end-of-comment ()
  (while (and (not (bobp))
              (save-excursion
                (forward-line 1)
                (back-to-indentation)
                (dialog--looking-at-comment-p)))
    (forward-line 1))
  (end-of-line))

(defun dialog--looking-at-blank-p (n)
  "Returns true if point is on a blank line.
Moves N lines forward first (backward if N is negative or to line
begin if N is 0)."
  (save-excursion
    (when (not (= n 0)) (forward-line n))
    (forward-line 0)
    (looking-at "^\s*$")))

(defun dialog-insert-comment-divider ()
  "Inserts a % comment divider dialog-fill-column wide."
  (interactive)
  (let ((blank-before (dialog--looking-at-blank-p -1))
        (blank-on (dialog--looking-at-blank-p 0)))
    (unless blank-before
      (forward-line 0)
      (open-line 2)
      (forward-line 1))
    (insert-char ?% dialog-fill-column)
    (unless blank-on
      (open-line 2))))

;;;###autoload
(define-derived-mode dialog-mode text-mode "Dialog"
  "Major mode for editing Dialog interactive fiction files.

\\{dialog-mode-map}"
  :syntax-table dialog-mode-syntax-table

  (setq-local indent-tabs-mode t)
  (setq-local tab-width dialog-tab-width)
  (setq-local fill-column dialog-fill-column)

  (setq-local comment-start "%% ")
  ;; This regexp tells Emacs where the "actual text" begins on a comment line.
  (setq-local comment-start-skip "\\(?:%%+\\s-*\\)")

  ;; This tells the paragraph motion commands that a line containing
  ;; ONLY a comment starter (and optional whitespace) is a paragraph boundary.
  ;; This prevents the filling engine from merging lines separated by a '%%' line.
  (setq-local paragraph-separate (concat "^\\s-*%%+\\s-*$\\|" paragraph-separate))
  (setq-local paragraph-start (concat "^\\s-*%%+\\s-*$\\|" paragraph-start))

  (setq-local font-lock-defaults '(dialog-font-lock-keywords))

  (setq-local imenu-generic-expression
              `((nil "^\\s-*\\(#" dialog-identifier "\\)" 1)
                (nil "^%% \\([A-Z].*[^\n]\\)" 1)))
  (imenu-add-to-menubar "Story")

  ;;(setq-local fill-paragraph-function #'dialog-fill-paragraph)

  ;; Add hooks with DEPTH `nil' and LOCAL `t' to ensure they only
  ;; trigger in Dialog buffers.
  (add-hook 'xref-backend-functions #'dialog-xref-backend nil t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dg\\'" . dialog-mode))

(provide 'dialog-mode)

;;; dialog-mode.el ends here
