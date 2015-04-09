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
    "DUMMY"
    "DATA" "OCOPY" "BINARY"

    "PATHDISP"
    "KEEP" "DELETE"

    "PATHOPTS"
    "OWRONLY" "OCREAT" "OTRUNC"

    "PATHMODE"

    "PUNCH" "PATH" "UNIT" "MSGLEVEL" "CLASS" "MSGCLASS" "DSN" "PGM"
    "REGION" "OLD" "PASS"

    ;; job names
    "S06COP" "S04COP" "S08COP" "S07FTP" "S03MBS" "S09DEL" "S05MAK"
    "STDOUT" "STDERR" "STDIN" "STDENV"
    "HFSOUT" "HFSERR" "JESOUT" "JESERR"

    ;; DDNAME
    "SYSTSIN"
    "SYSIN" ; passing parameters, if not used set to DUMMY
    "SYSOUT" ; output file for messages from a given utillity
    "SYSUT1" ; input file
    "SYSUT2" ; output file
    "SYSPRINT" ; output file for printed output from a given utility
               ; i.e. can show number of processed records, condition codes etc.
    "SYSTSPRT"
    "SYSDUMP"
    "SYSUDUMP" ; output file for a system 'dump' if the program fails
    "DD1"
    "SCRIPT"

    "NUCL" "TMEIIN" "TMEIOUT" "EIAPIN" "EIAPOUT" "TMMAIN" "TMMAOUT" "TM12IN"
    "MA12IN" "MA12OUT" "SU12IN" "SU12OUT" "GROUPS"
    "OPTRPT"
    "DATASUM" "CONTENTS" "GRPRPT" "SUMMRY" "DETAIL" "RESOURCE" "OPTIONS"
    "SYSDA" "SPACE" "TRK" "DISP" "VOL" "REF" "DCB" "BLKSIZE" "LRECL" "RECFM"
    "RLSE"

    ;; my steps
    "STEP1" "STEP01"
    "STEP2" "STEP02"
    "STEP3" "STEP02"
    "STEP4" "STEP04"
    "STEP5" "STEP05"

    "STEPLIB" "DATAIN"
    "DDNAME"
    "ROUTE"
    "USSCMD"
    "INDD" "OUTDD" "PARM" "COND" "BPXBATCH"
    "IKJEFT01"
    "BPXJCL"

    "NEW" "CATLG" "DSORG"
    "NOTIFY"

    "COPY"
    "SMTP" ; emailing
    ))

(defconst jcl-constants
  '(
    "FB" "VB" "LT" "H" "A"

    "IEFBR14" ; datasets: create / delete:
              ; PS (Physical Sequential) / PDS (Partitioned) / temporary

    "IEBCOPY" ; IBM utility to copy partitioned dataset (PDS) including members
    "IEBGENER" ; IBM utility to copy Physical Sequential files
               ; records with max length 32760 bytes
               ; can copy PDS to PS
               ; can send: emails / files to printer
    "PS"       ; Physical Sequential Dataset - file
    "PO"       ; Create Dataset PDS member - directory
    "SIRUSR" "SIWUSR" "SIRWXU"
    "ORDONLY"
    "SHR"
    ))

(defconst jcl-preprocessor
  '(
    ;; 1. main task - job name
    "JOB"
    ;; 1.1. main task is dividen into subtasks - activity name
    "EXEC"
    ;; 1.1.1 subtasks are dividen into datasets - dd name
    "DD"

    "ADN0035"
    "SYSUID"
    "TEMDS" ; Temporary Pysical Squential file
    "SEND"
    ))

(defvar jcl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'jcl-do-foo)
    map)
  "Keymap for `jcl-mode'.")

(defvar jcl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\' "\"'" st)   ; strings enclosed in single quotes
    (modify-syntax-entry ?\" "\"\"" st)  ; strings enclosed in double quotes
    st)
  "Syntax table for `jcl-mode'.")

(defconst jcl-font-lock-keywords
  (list
   ;; statement
   (cons "\\(^\\/\\.\\)" 'font-lock-preprocessor-face) ; label /.
   (cons "\\(^\\/&\\)" 'font-lock-preprocessor-face) ; end-of-job /&
   (cons "\\(^\\/\\*\\)" 'font-lock-preprocessor-face) ; end-of-data /*
   (cons "\\(^\\/\\+\\)" 'font-lock-preprocessor-face) ; end-of-procedure /+

   (cons "\\(\\/\\/\\*.*\\)" 'font-lock-comment-face) ; comment starts with: //*
   (cons (regexp-opt jcl-keywords 'words) 'font-lock-keyword-face)
   (cons (regexp-opt jcl-constants 'words) 'font-lock-constant-face)
   (cons (regexp-opt jcl-preprocessor 'words) 'font-lock-preprocessor-face)
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
  "A major mode for editing Job Control Language (JCL) scripts."
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
