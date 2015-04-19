;;; jcl.el --- Jcl major mode

;; Copyright (C) 2001  Free Software Foundation, Inc.
;;               2015  Rostislav Svoboda

;; Author: Rostislav Svoboda
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
(defconst jcl-step-names
  (list "STEP01" "STEP02" "STEP03" "STEP04" "STEP05"))

(defconst jcl-keywords
  (list
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
   "SYSDA" "SPACE" "TRK" "DISP" "VOL" "REF"
   "DCB" ; Data Control Block
   "BLKSIZE" "LRECL" "RECFM"
   "RLSE"

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

(defconst jcl-programs
  (list
   "IEFBR14" ; datasets: create / delete:
                                        ; PS (Physical Sequential) / PDS (Partitioned) / temporary

   "IEBCOPY" ; IBM utility to copy partitioned dataset (PDS) including members
   "IEBGENER" ; IBM utility to copy Physical Sequential files
                                        ; records with max length 32760 bytes
                                        ; can copy PDS to PS
                                        ; can send: emails / files to printer
   ))

(defconst jcl-constants
  (list
   "FB" "FBA"     ; Fixed Block Size (with ASCII control chars)
   "VB" "VBA"     ; Variable Block Size (with ASCII control chars)
   "LT" "H" "A"

   ;; Dataset Organisation (DSORG):
   "PS"       ; DSORG: Physical Sequential fataset - file
   "PO"       ; DSORG: Partitioned Organized dataset - directory
   "SIRUSR" "SIWUSR" "SIRWXU"
   "ORDONLY"
   ;; DSN=OLDFILE,DISP=SHR signalizes that OLDFILE already exists
   ;; and can be used by other programs while the job is running.
   "SHR"
   ))

(defun jcl-statements ()
  (list
   ;; statement; identifies the start of the job; information about the whole job:
   ;; billing / run priority / time / space limits
   "JOB"
   ;; statement; identifies program to be executed in this step; information about the step.
   "EXEC"
   ;; Data Definition / Description statements; identify a data file to be used
   ;; detailed info about that file. DD statements can be in any order within the step.
   "DD"

   ;; "&SOMETING" - the SOMETHING will be specified at the runtime
   ;; //ADN0035  JOB (123),... - 123 is the billing information

   ;; X in the column 72 followed by // in the next line extends comments

   ;; Contition Codes: 0: Normal, 4: Warn, 8: Error, 12: Severe Error, 16: Terminal Error
   "COND"
   
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
   ;; 5 fields of a JCL statement:
   ;; Identifier-Field Name-Field Operation-Field Parameter-Field Comments-Field
   ;;                 ^          ^               ^               ^
   ;;              no space     space          space           space

   (cons "\\(^\\/\\.\\)" 'font-lock-preprocessor-face) ; label /.
   (cons "\\(^\\/&\\)" 'font-lock-preprocessor-face) ; end-of-job /&
   (cons "\\(^\\/\\*\\)" 'font-lock-preprocessor-face) ; end-of-data /*
   (cons "\\(^\\/\\+\\)" 'font-lock-preprocessor-face) ; end-of-procedure /+
   (cons "\\(\\/\\/\\*.*\\)" 'font-lock-comment-face) ; comment starts with: //*

   (cons (regexp-opt jcl-keywords 'words)   'font-lock-keyword-face)
   (cons (regexp-opt jcl-step-names 'words) 'font-lock-keyword-face)
   
   (cons (regexp-opt jcl-constants 'words) 'font-lock-constant-face)

   (cons (regexp-opt (jcl-statements) 'words) 'font-lock-preprocessor-face)
   (cons (regexp-opt jcl-programs 'words)     'font-lock-preprocessor-face)
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
                   (format-time-string "%Y-%m-%dT%T"))))

(defun jcl-shell-buffer (jcl-buffer-name)
  (let* ((delim "*")
         (shell-buffer-prefix
          "shell-ftp"))
    (concat delim shell-buffer-prefix
            ;; "-" jcl-buffer-name
            delim)))

(defun jcl-pds-names (jcl-buffer-name)
  (switch-to-buffer jcl-buffer-name)
  (let* (
         (base-path (concat (getenv "HOME")
                            "/dev/mainframe/host/resources")) ; without / at the end

         (file-name (buffer-file-name (get-file-buffer (buffer-name))))
         (pds-path (substring file-name (length (concat base-path "/"))))
         (pds-names (split-string pds-path "/"))
         )
    ;; (message (concat
    ;;           "pds-names: "
    ;;           "1. "(first pds-names) ", 2. " (second pds-names)))
    pds-names))

(defun jcl-save-on-host (jcl-buffer-name ip-addr)
  (interactive)
  (let* (
         (base-path (concat (getenv "HOME")
                            "/dev/mainframe/host/resources")) ; without / at the end

         (shell-buffer (jcl-shell-buffer jcl-buffer-name))
         (file-name (buffer-file-name (get-file-buffer jcl-buffer-name)))

         (pds-names (jcl-pds-names jcl-buffer-name))
         (pds-name-1 (nth 0 pds-names))
         (pds-name-2 (nth 1 pds-names))
         )
    (if (get-buffer shell-buffer)
        (progn
          (switch-to-buffer shell-buffer)
          (goto-char (point-max))

          (insert (concat "open " ip-addr))
          (comint-send-input)
          )
      (progn
        (shell)

        ;; sftp takes me to Unix instead of Host
        (insert (concat "ftp -v " ip-addr))
        (comint-send-input)

        (rename-buffer shell-buffer)

        ;; TODO do not change the command order
        (insert "prompt")
        (comint-send-input)
        )
      )

    (insert (concat "lcd " base-path "/" pds-name-1 "/" pds-name-2))
    (comint-send-input)

    (insert "cd ~")
    (comint-send-input)

    ;; TODO 'cd ~.FOO.BAR' does not work
    (insert (concat "cd " pds-name-1 "." pds-name-2))
    (comint-send-input)

    (insert (concat "mput " jcl-buffer-name))
    (comint-send-input)
    )
  )

(defun jcl-close-shell-buffer (shell-buffer)
  (interactive)
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
      (message (concat "This buffer is not the " shell-buffer)))))

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
(add-hook 'cobol-mode-hook 'jcl-mode-keys)
(add-hook 'shell-mode-hook 'jcl-shell-mode-keys)

(provide 'jcl-mode)
;;; jcl.el ends here
