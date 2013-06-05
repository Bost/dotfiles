; TODO try evil

; only for windows or cygwin
(setq url-proxy-services
       '(("https" . "192.168.2.105:3128")
         ("http" . "192.168.2.105:3128")))

(setq inhibit-splash-screen)
;(load-theme 'deeper-blue)
(load-theme 'misterioso)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'auto-complete-config)
(ac-config-default)

(global-linum-mode t)

;; press y/n instead of typing yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; this is for the emacs code browser
(setq stack-trace-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/dev/webcli")))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;(desktop-load-default)
;;(desktop-read)

(require 'org-install)
(org-babel-do-load-languages
'org-babel-load-languages
'(
  (sh . t)
  (python .t)
  (R . t)
  (ruby . t)
  (ditaa . t)
  (dot . t)
  (sqlite . t)
  (perl . t)
))

; Add shortcuts for ogr-agenda
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

; Setup custom shortcuts
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)

;; TODO this must be set according to the environment
;(setq inferior-lisp-program "~/bin/lein repl")

;(add-to-list 'load-path "~/.emacs.d/edit-server/")
;(require 'edit-server)
;(edit-server-start)

(autoload 'magit-status "magit" nil t)

;; change font size
(set-face-attribute 'default nil :height 110)

;; show line number on the left side
(global-linum-mode t)

;; highlight current line - this is probably not needed in the default face
;(global-hl-line-mode 1)
(column-number-mode 1)

;;; Change the path to db2cmd to reflect the correct
;;; location for your machine.
(setq sql-db2-program "/cygdrive/c/winapp/IBM/SQLLIB_9.5/BIN/db2cmd.exe")

;;; The interesting options here is the "-t" option
;;; passed to "db2". This is extremely handy - it
;;; means that ';' (semicolon) ; is treated as the command
;;; line terminator. The default is to treat the end-of-line
;;; as a SQL statement terminator.
;;; You may look up the command reference online for an
;;; explanation of the rest.
(setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))

;; no line wrap
(setq default-truncate-lines t)

(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)
(global-set-key [f9] 'other-window)

(defun exec-sql ()
  ;;(windmove-up)
  (save-buffer)
  (windmove-down)
  (goto-char (point-max))
  (interactive)
  (insert "db2 -vf c:\\cygwin\\home\\svo02896\\dev\\txrating\\tmp.sql -t")
  (eshell-send-input)
  (windmove-up))

(defun comment-sql-line ()
  (setq current-point (point))
  (move-beginning-of-line nil)
  (insert "--")
  (goto-char current-point))

(defun clean-whitespaces ()
  (setq current-point (point))
  (goto-char (point-min))
  (replace-regexp "\t" "    ")
  (goto-char (point-min))
  (replace-regexp "[\t ]+$" "")
  (save-buffer)
  (goto-char current-point))


;; some other not used keys
;(global-set-key [f11] 'comment-sql-line)
(global-set-key [f12] 'exec-sql)
;; (global-set-key [scroll] 'exec-test-macro)

;; buffer switching
;; (iswitchb-mode t)			; I think this is the default
(ido-mode t)				; probably somehow better

;; (desktop-save-mode 1)

(defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file "~/.emacs")                   ; open ~/.emacs or other user init file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer

;; setting the PC keyboard's various keys to
;; Super or Hyper, for emacs running on Windows.
;; run Local Group Policy Editor (gpedit.msc) -> User Configuration
;; -> Administrative Templates -> Windows Components -> Windows Explorer
;; -> Turn off Windows+X hotkeys, set it to 'Not configured' and log off
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

(global-set-key (kbd "M-s") 'save-buffer)
;; s-s is here just to have consistent key mapping.
;; If it's gonna work I can use M-s for something else
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-c") 'kill-ring-save) ; copy
(global-set-key (kbd "s-x") 'kill-region)    ; cut
(global-set-key (kbd "s-v") 'yank)	     ; paste
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-k") 'ido-kill-buffer)


(global-set-key (kbd "C-S-k") 'kill-line)
(global-set-key [C-S-delete] 'kill-line) ; the same as in eclipse
;; C-S-backspace is the default key binding for kill-whole-line
(global-set-key (kbd "C-k") 'kill-whole-line)
(put 'upcase-region 'disabled nil)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))
