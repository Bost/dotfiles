(require 's)

(when (s-ends-with? "VirtualBox" system-name)
  (progn
    (use-package jcl
      :load-path "~/dev/dotfiles/jcl")

    (use-package cobol-mode
      :load-path "~/dev/dotfiles/jcl")

    (use-package rexx-mode
      :load-path "~/.emacs.d/rexx-mode")
    ))

(if (string= system-type "windows-nt")
    (progn
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

      (defun get-font-height () ;; font size
        (interactive)
        (cond
         ((s-ends-with? "VirtualBox" system-name) 102)
         ((string= system-type "windows-nt") 102)
         (t 102)))

      (when (string= system-name "VirtualBox")
        (display-battery-mode 1))

      (defun find-file-emacs ()
        (interactive)
        (find-file "~/.emacs"))

      (load "~/bin/dbases.el"))
  (progn
    ;; This works only when bash environment initialised.
    ;; I.e. invoke emacs from CLI or modify emacs24 xfce launcher:
    ;; bash -c -i ~/dev/emacs/src/emacs
    (defun get-font-height () ; font size
      (interactive)
      (cond
       ((> (string-to-number (getenv "isLinuxNew64")) 0) 116)
       ((> (string-to-number (getenv "isLinuxFranzi")) 0) 130)
       ((> (string-to-number (getenv "isLinuxMartinJV")) 0) 120)
       ((> (string-to-number (getenv "isLinuxVB")) 0) 102)
       (t 140)))

    (if (> (string-to-number (getenv "isLinuxFranzi")) 0)
        (display-battery-mode 1))

    (defun find-file-emacs ()
      (interactive)
      (find-file "~/dev/dotfiles/.emacs"))))
