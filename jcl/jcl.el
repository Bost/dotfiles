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

    ;; Data Definition (DD) names:
    "SYSTSIN"
    "SYSIN" ; DD contains parameters for PGM utility, if not used set to DUMMY
    "SYSOUT" ; output DD for messages from PGM utillity
    "SYSUT1" ; input DD
    "SYSUT2" ; output DD
    "SYSPRINT" ; output DD  for printed output (info messages / errors) from PGM utility
               ; i.e. can show number of processed records, condition codes etc.
    "SYSTSPRT"
    "SYSDUMP"
    "SYSUDUMP" ; output DD for a system 'dump' if the PGM utility fails
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
    "FB" "FBA"     ; Fixed Block Size (with ASCII control chars)
    "VB" "VBA"     ; Variable Block Size (with ASCII control chars)
    "LT" "H" "A"

    "IEFBR14" ; datasets: create / delete:
              ; PS (Physical Sequential) / PDS (Partitioned) / temporary

    "IEBCOPY" ; IBM utility to copy partitioned dataset (PDS) including members
    "IEBGENER" ; IBM utility to copy Physical Sequential files
               ; records with max length 32760 bytes
               ; can copy PDS to PS
                                        ; can send: emails / files to printer
    ;; Dataset Organisation (DSORG):
    "PS"       ; DSORG: Physical Sequential fataset - file
    "PO"       ; DSORG: Partitioned Organized dataset - directory
    "SIRUSR" "SIWUSR" "SIRWXU"
    "ORDONLY"
    "SHR"      ; signalizes - dataset already exists, and can be used by other programs while the job is running.
    ))

(defun jcl-preprocessor ()
  (list
    ;; 1. main task - job name
    "JOB"
    ;; 1.1. main task is dividen into subtasks - activity name
    "EXEC"
    ;; 1.1.1 subtasks are dividen into datasets - dd name
    "DD" ; Data Definition

    (getenv "HOST_USERNAME")
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
   (cons (regexp-opt (jcl-preprocessor) 'words) 'font-lock-preprocessor-face)
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

(defun jcl-shell-buffer (buffer-name)
  (let* ((delim "*")
         (shell-buffer-prefix
          "shell-ftp"))
    (concat delim shell-buffer-prefix
            ;; "-" buffer-name
            delim)))

(defun jcl-save-on-host (buffer-name ip-addr)
  (interactive)

  (let* ((shell-buffer (jcl-shell-buffer buffer-name)))
    (if (get-buffer shell-buffer)
        (progn
          (switch-to-buffer shell-buffer)
          (goto-char (point-max))
          (insert (concat "open " ip-addr))
          (comint-send-input)
          )
      (progn
        (shell)

        (insert "cd ~/dev/mainframe/host/resources/RACFBK/CNLT")
        (comint-send-input)

        ;; sftp takes me to Unix instead of Host
        (insert (concat "ftp -v " ip-addr))
        (comint-send-input)

        (rename-buffer shell-buffer)

        (insert "cd RACFBK.CNTL")
        (comint-send-input)

        (insert "prompt")
        (comint-send-input)
        )
      )
    (insert (concat "mput " buffer-name))
    (comint-send-input)
    )
  )

(defun jcl-close-shell-buffer (buffer-name)
  (interactive)
  (let* ((shell-buffer (jcl-shell-buffer buffer-name)))
    (if (equal shell-buffer (buffer-name (current-buffer)))
        (progn
          (end-of-buffer)
          (insert "quit")
          (comint-send-input) ; works even in evil normal mode
          (insert "exit")
          (comint-send-input)
          (if (get-buffer-process shell-buffer)
              (progn
                (let* ((timeout 500))
                  (message (concat "shell-buffer: " shell-buffer ": "
                                   "Waiting " (number-to-string timeout)
                                   "ms for remaining process(es) to terminate"))
                  (sleep-for 0 timeout))))
          (message (concat "Closing buffer: " shell-buffer))
          (close-buffer)
          )
      (progn
        (message (concat "This buffer is not the " shell-buffer))))
    ))

(defun jcl-save-on-host-buffer ()
  (interactive)
  (jcl-save-on-host (buffer-name) (getenv "IP_SDVE")))

(defun jcl-upcase-buffer ()
  "TODO do not upcase comments"
  (interactive)
  (upcase-region (point-min) (point-max)))

(defun jcl-close-shell-buffer-any ()
  (interactive)
  (jcl-close-shell-buffer (jcl-shell-buffer nil)))

(defun jcl-shell-mode-keys ()
  "Modify keymaps used by `shell-mode'."
  (local-set-key (kbd "s-k") 'jcl-close-shell-buffer-any)
  )

(defun jcl-mode-keys ()
  "Modify keymaps used by `jcl-mode'."
  (local-set-key (kbd "s-r") 'jcl-reload)
  (local-set-key (kbd "s-l") 'jcl-save-on-host-buffer)
  (local-set-key (kbd "s-u") 'jcl-upcase-buffer)
  )

(add-hook 'jcl-mode-hook 'jcl-mode-keys)
(add-hook 'shell-mode-hook 'jcl-shell-mode-keys)

(provide 'jcl)
;;; jcl.el ends here
