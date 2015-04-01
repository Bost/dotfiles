;;; jcl.el --- Jcl major mode

 ;; Copyright (C) 2015

 ;; Author:
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
(require 'generic-x) ;; we need this

(defvar jcl-tab-width nil "Width of a tab for Jcl mode")

(defconst jcl-font-lock-keywords-1
  (list
   '("\\(?:BINARY\\|D\\(?:ATA\\|D\\|ELETE\\|UMMY\\)\\|EXEC\\|KEEP\\|O\\(?:C\\(?:OPY\\|REAT\\)\\|TRUNC\\|WRONLY\\)\\|PATH\\(?:DISP\\|MODE\\|OPTS\\)\\|SI\\(?:R\\(?:USR\\|WXU\\)\\|WUSR\\)\\)" . font-lock-builtin-face)
   ;; '("\\('\\w*'\\)" . font-lock-variable-name-face)
   )
  "Minimal highlighting expressions for Jcl mode")

(defvar jcl-font-lock-keywords jcl-font-lock-keywords-1
  "Default highlighting expressions for Jcl mode")

;; (regexp-opt '(
;;     "DD" "DUMMY" "EXEC" "DATA" "OCOPY" "BINARY"

;;     "PATHDISP"
;;     "KEEP" "DELETE"

;;     "PATHOPTS"
;;     "OWRONLY" "OCREAT" "OTRUNC"

;;     "PATHMODE"
;;     "SIRUSR" "SIWUSR" "SIRWXU"))

(define-generic-mode 'jcl-mode
  ()
  ;; ()
  '(
    "DD" "DUMMY" "EXEC" "DATA" "OCOPY" "BINARY"

    "PATHDISP"
    "KEEP" "DELETE"

    "PATHOPTS"
    "OWRONLY" "OCREAT" "OTRUNC"

    "PATHMODE"
    "SIRUSR" "SIWUSR" "SIRWXU"
    )
  '(
    ("\\(\\/\\/\\*.*\\)" 1 'font-lock-comment-face) ; comment starts with: //*
    ("\\(\\/\\/[^\\*].*\\)" 1 'font-lock-preprocessor-face) ; statement (OS processes): //
    ;; ("\\(\\/\\/[A..Z]\\)" 1 'font-lock-preprocessor-face) ; statement (OS processes): //
    ;; ("\\(\\/\\*\\)" 1 'font-lock-comment-face) ; end of in stream data
    )
  ;; '("\\.jcl$")                      ;; files for which to activate this mode
  ()
  (list (lambda ()
          ;; (set (make-local-variable 'comment-start) "\//\*")
          (set (make-local-variable 'comment-start) "\/\/\* ")
          ;; (set (make-local-variable 'comment-end) "")
          ;; (set (make-local-variable 'font-lock-defaults) '(jcl-font-lock-keywords))
          ))
  "JCL mode is a major mode for editing JCL files")

(add-to-list 'auto-mode-alist '("\\.jcl\\'" . jcl-mode))

(provide 'jcl)
