;;; ren.el --- Ren major mode

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: StefanMonnier
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

 ;;; Commentary:

;;

 ;;; Code:

(defvar ren-mode-hook nil)

(defvar ren-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map "\C-m" 'newline-and-indent)
    map)
  "Keymap for `ren-mode'.")

(defvar ren-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "w 1"  table)
    (modify-syntax-entry ?\  "  2b" table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\' "w"    table)
    (modify-syntax-entry ?-  "w"    table)
    (modify-syntax-entry ?\: "w"    table)
    (modify-syntax-entry ?\; "w"    table)
    ;; (modify-syntax-entry ?\: "(;"   table)
    ;; (modify-syntax-entry ?\; "):"   table)
    (modify-syntax-entry ?,  "w"    table)
    (modify-syntax-entry ?_  "w"    table)
    (modify-syntax-entry ?!  "w 2b" table)
    (modify-syntax-entry ?\( "()"   table)
    (modify-syntax-entry ?\) ")("   table)
    (modify-syntax-entry ?\{ "(}"   table)
    (modify-syntax-entry ?\} "){"   table)
    (modify-syntax-entry ?\[ "(]"   table)
    (modify-syntax-entry ?\] ")["   table)
    table)
  "Syntax table for `ren-mode'.")

;;
(defvar ren-font-lock-keywords
  `(;; ("\\(^\\| \\):[^ ]* +[^ ]+" . font-lock-function-name-face)
    ;; ("a?\"[^\"]+\"" . font-lock-string-face)
    ("\\<[A-Z_][^ ]*" . font-lock-variable-name-face)
    (,(regexp-opt '(":" ";" "immediate" "true" "false" ":g" ":m"
                    "((" "))" "(" ")" "{" "#{" "}" "[" "]") t) . font-lock-constant-face))
  "Keyword highlighting specification for `ren-mode'.")

(defvar ren-imenu-generic-expression
  nil
  )

(defvar ren-outline-regexp
  nil
  )

(defconst ren-smie-token-regexp
  "^:[gm]?\\|;")

(defconst ren-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp)
      (word)
      (cmd (":" word exp ";"))))))

(defvar ren-indent-width 4)

(defun ren-smie-rules (kind token)
  (let ((x (pcase (cons kind token)
             (`(:after . ":")  ren-indent-width)
             (`(:after . ";")  '(column . 0))
             ;; (`(:before . ,(or `":" `";")) '(column . 0))
             ;; (`(:elem . basic) ren-indent-width)
             (`(:list-intro . ,_) ren-indent-width)
           )))
    (message (format "rules: [%s] [%s] [%s]" kind token x))
    x))

(defconst ren-token-regexp
  "\\sw+"
  ;;   "((\\|[][{}():;]\\|case"
  )

(defun ren-smie-forward-token ()
  (forward-comment (point-max))
  (let ((x (buffer-substring-no-properties
            (point)
            (cond ((looking-at ren-token-regexp)
                   (goto-char (match-end 0)))
                  (t (skip-syntax-forward "w_")
                     (point))))))
    (message (format "forward: [%s]" x))
    x))

(defun ren-smie-backward-token ()
  (forward-comment (- (point)))
  (let ((x (cond ((looking-back ren-token-regexp (- (point) 20) t)
                  (goto-char (match-beginning 0))
                  (match-string-no-properties 0))
                 (t
                  (buffer-substring-no-properties
                   (point)
                   (progn (skip-syntax-backward "w_")
                          (point)))))))
    (message (format "backward: [%s]" x))
    x))


;;;###autoload
(define-derived-mode ren-mode prog-mode "Ren"
  "A major mode for editing Ren files."
  :syntax-table ren-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "# ")
  (setq-local font-lock-defaults '(ren-font-lock-keywords))
  (setq-local imenu-generic-expression
              ren-imenu-generic-expression)
  (setq-local outline-regexp ren-outline-regexp)
  (setq-local indent-line-function 'ren-indent-line)
  ;; (smie-setup ren-smie-grammar 'ren-smie-rules
  ;;             :forward-token 'ren-smie-forward-token
  ;;             :backward-token 'ren-smie-backward-token)
  )

(defun ren-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
                       (beginning-of-line)
                       (if (>= (point) savep) (setq savep nil))
		       (max (ren-calculate-indentation) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun ren-calculate-indentation ()
  (cond ((bobp) 0)
        ((or (looking-at "^[ \t]*;")
             (looking-at "^[ \t]*[]})]$"))
         (forward-line -1)
         (- (current-indentation) ren-indent-width))
        (t
         (let ((not-indented t)
               (cur-indent 0))
           (while not-indented
             (forward-line -1)
             (cond ((looking-at "\\(^\\|.* \\);[^ ]")
                    (setq not-indented nil)
                    (setq cur-indent (current-indentation)))
                   ((looking-at "\\(^\\|.* \\);")
                    (setq not-indented nil))
                   ((or (looking-at "^[ \t]*\\(:[gm]?\\|case\\|receive\\)")
                        (looking-at ".*[{([][ \t]*$"))
                    (setq cur-indent (+ (current-indentation) ren-indent-width))
                    (setq not-indented nil))
                   ((bobp)
                    (setq not-indented nil))))
           cur-indent))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ren$" . ren-mode))

(provide 'ren-mode)
 ;;; ren-mode.el ends here
