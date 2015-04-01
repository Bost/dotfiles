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

;; identifier job-name operands params
(defconst jcl-keywords
  '(
    ;; 1. main task - job name
    "JOB"
    ;; 1.1. main task is dividen into subtasks - activity name
    "EXEC"
    ;; 1.1.1 subtasks are dividen into datasets - dd name
    "DD"

    "DUMMY"
    "DATA" "OCOPY" "BINARY"

    "PATHDISP"
    "KEEP" "DELETE"

    "PATHOPTS"
    "OWRONLY" "OCREAT" "OTRUNC"

    "PATHMODE"

    "PUNCH" "PATH" "UNIT" "JOB" "MSGLEVEL" "CLASS" "MSGCLASS" "DSN" "PGM"
    "REGION" "OLD" "PASS"

    ;; job names
    "S06COP" "S04COP" "S08COP" "S07FTP" "S03MBS" "S09DEL" "S05MAK"
    "STDOUT" "STDERR" "STDIN" "STDENV"
    "HFSOUT" "HFSERR" "JESOUT" "JESERR"
    "SYSTSIN" "SYSTSPRT"
    "SCRIPT"

    "NUCL" "TMEIIN" "TMEIOUT" "EIAPIN" "EIAPOUT" "TMMAIN" "TMMAOUT" "TM12IN"
    "MA12IN" "MA12OUT" "SU12IN" "SU12OUT" "GROUPS" "SYSOUT" "SYSPRINT" "OPTRPT"
    "DATASUM" "CONTENTS" "GRPRPT" "SUMMRY" "DETAIL" "RESOURCE" "OPTIONS"
    "SYSDA" "SPACE" "TRK" "DISP" "VOL" "REF" "DCB" "BLKSIZE" "LRECL" "RECFM"
    "RLSE" "STEP1" "STEPLIB" "DATAIN"
    "ROUTE"
    "USSCMD"
    "INDD" "OUTDD" "PARM" "COND" "BPXBATCH"
    "IKJEFT01"
    "BPXJCL"
    ))

(defconst jcl-constants
  '(
    "FB" "VB" "LT"
    "SIRUSR" "SIWUSR" "SIRWXU"
    "ORDONLY"
    ))

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

(defconst jcl-font-lock-keywords
  (list 
   ;; ("\"\\.\\*\\?" . font-lock-string-face)

   ;; statement
   (cons "\\(^\\/\\.\\)" 'font-lock-preprocessor-face) ; label /.
   (cons "\\(^\\/&\\)" 'font-lock-preprocessor-face) ; end-of-job /&
   (cons "\\(^\\/\\*\\)" 'font-lock-preprocessor-face) ; end-of-data /*
   (cons "\\(^\\/\\+\\)" 'font-lock-preprocessor-face) ; end-of-procedure /+

   (cons "\\(\\/\\/\\*.*\\)" 'font-lock-comment-face) ; comment starts with: //*
   ;; ("function \\(\\sw+\\)" (1 font-lock-function-name-face))
   (cons
    (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"")
    'font-lock-string-face)
   (cons
    (rx "'" (group (0+ (or (1+ (not (any "'" "\\"))) (seq "\\" anything)))) "'")
    'font-lock-string-face)
   (cons (regexp-opt jcl-keywords 'words) 'font-lock-keyword-face)
   (cons (regexp-opt jcl-constants 'words) 'font-lock-constant-face)
   ;; numbers
   (cons "\\<\\(\\+\\|-\\)?[0-9]+\\(\\.[0-9]+\\)?\\>" 'font-lock-constant-face)
   )
  "Keyword highlighting specification for `jcl-mode'.")

(defvar jcl-imenu-generic-expression
  nil)

(defvar jcl-outline-regexp
  nil)

;;;###autoload
(define-derived-mode jcl-mode fundamental-mode "jcl-script"
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jcl$" . jcl-mode))

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

(defun jcl-reload ()
  (interactive)
  (eval-buffer "jcl.el")
  ;; (fundamental-mode)
  (jcl-mode)
  (message (concat "jcl-mode evaluated & reloaded: "
                   (format-time-string "%Y-%m-%dT%T")))
  )
(defun jcl-mode-keys ()
  "Modify keymaps used by `jcl-mode'."
  (local-set-key (kbd "s-r") 'jcl-reload))

(add-hook 'jcl-mode-hook 'jcl-mode-keys)

(provide 'jcl)
;;; jcl.el ends here
