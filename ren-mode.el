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
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?#  ". 1"  st)
    (modify-syntax-entry ?\  ". 2b" st)
    (modify-syntax-entry ?\n "> b"  st)
    (modify-syntax-entry ?\' "'"    st)
    (modify-syntax-entry ?!  ". 2b" st)
    st)
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

(defconst ren-smie-tokens
  '((":" . (":" ":g" ":m"))
    (keyword  . (";" "immediate"))))

(defconst ren-smie-token-regexp
  "^:[gm]?\\|;")

(defconst ren-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp)
      (cmd
       (":" words ";")
       (":g" words ";")
       (":m" words ";")))
    '((assoc ";")))))

(defvar ren-indent-width 4)

(defun ren-smie-rules (kind token)
  (message (format "[%s] [%s]" kind token))
  (pcase (cons kind token)
    (`(:after . ";")  '(column . 0))
    (`(:after . ":")  ren-indent-width)
    (`(:before . ":") '(column . 0))
    (`(:elem . basic) ren-indent-width)
    (`(:list-intro . ,(or "")) t)))

(defun ren-smie-forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (cond ((looking-at "[:;]") (forward-char 1) (point))
         (t (skip-syntax-forward "w_")
            (point)))))

(defun ren-smie-backward-token ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (cond ((looking-back "[:;]") (forward-char -1) (point))
         (t (skip-syntax-backward "w_")
            (point)))))


;;;###autoload
(define-derived-mode ren-mode fundamental-mode "Ren"
  "A major mode for editing Ren files."
  :syntax-table ren-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "# ")
  (setq-local font-lock-defaults
              '(ren-font-lock-keywords))
  ;; (setq-local indent-line-function 'ren-indent-line)
  (setq-local imenu-generic-expression
              ren-imenu-generic-expression)
  (setq-local outline-regexp ren-outline-regexp)
  (smie-setup ren-smie-grammar 'ren-smie-rules
              :forward-token 'ren-smie-forward-token
              :backward-token 'ren-smie-backward-token)
  )

 ;;; Indentation

(defun ren-indent-line ()
  "Indent current line of Ren code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (ren-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun ren-calculate-indentation ()
  "Return the column to which the current line should be indented."
  ;; ...
  )


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ren$" . ren-mode))

(provide 'ren-mode)
 ;;; ren-mode.el ends here
