;;; jcl.el --- Jcl major mode

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

;; (regexp-opt '(
;;     "DD" "DUMMY" "EXEC" "DATA" "OCOPY" "BINARY"

;;     "PATHDISP"
;;     "KEEP" "DELETE"

;;     "PATHOPTS"
;;     "OWRONLY" "OCREAT" "OTRUNC"

;;     "PATHMODE"
;;     "SIRUSR" "SIWUSR" "SIRWXU"

;;     "PUNCH" "PATH" "UNIT" "JOB" "MSGLEVEL" "CLASS" "MSGCLASS" "DSN" "PGM" "REGION"))


;; (defvar jcl-mode-map nil)
(defvar jcl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'jcl-do-foo)
    map)
  "Keymap for `jcl-mode'.")

;; (defvar jcl-mode-syntax-table nil)
(defvar jcl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `jcl-mode'.")

;; (defvar jcl-font-lock-keywords nil)
(defvar jcl-font-lock-keywords
  '(
    ;; ("\\(\\/\\/\\*.*\\)" (1 font-lock-comment-face)) ; comment starts with: //*
    ("function \\(\\sw+\\)" (1 font-lock-function-name-face))
    ;; ("\\(?:BINARY\\|CLASS\\|D\\(?:ATA\\|D\\|ELETE\\|SN\\|UMMY\\)\\|EXEC\\|JOB\\|KEEP\\|MSG\\(?:CLASS\\|LEVEL\\)\\|O\\(?:C\\(?:OPY\\|REAT\\)\\|TRUNC\\|WRONLY\\)\\|P\\(?:ATH\\(?:DISP\\|MODE\\|OPTS\\)?\\|GM\\|UNCH\\)\\|REGION\\|SI\\(?:R\\(?:USR\\|WXU\\)\\|WUSR\\)\\|UNIT\\)" (1 font-lock-builtin-face))
    )
  "Keyword highlighting specification for `jcl-mode'.")

(defvar jcl-imenu-generic-expression
  nil)

(defvar jcl-outline-regexp
  nil)

 ;;;###autoload
(define-derived-mode jcl-mode fundamental-mode "Jcl"
  "A major mode for editing Jcl files."
  :syntax-table jcl-mode-syntax-table
  (setq-local comment-start "\/\/\* ")
  ;; (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(jcl-font-lock-keywords))
  (setq-local indent-line-function 'jcl-indent-line)
  (setq-local imenu-generic-expression
              jcl-imenu-generic-expression)
  (setq-local outline-regexp jcl-outline-regexp)
  )

 ;;; Indentation

(defun jcl-indent-line ()
  "Indent current line of Jcl code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (jcl-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun jcl-calculate-indentation ()
  "Return the column to which the current line should be indented."
  nil)

(defun jcl-mode-keys ()
  "Modify keymaps used by `jcl-mode'."
  (local-set-key (kbd "s-r") '(lambda ()
                                (interactive)
                                (eval-buffer "jcl.el")
                                ;; (fundamental-mode)
                                (jcl-mode)
                                (message (concat "jcl-mode evaluated & reloaded: "
                                                 (format-time-string "%Y-%m-%dT%T")))
                                )))

(add-hook 'jcl-mode-hook 'jcl-mode-keys)

(provide 'jcl-mode)
 ;;; jcl.el ends here
