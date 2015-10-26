(require 's)

(when (s-ends-with? "VirtualBox" system-name)
  (progn
    (use-package jcl-mode
      :load-path "~/dev/dotfiles/jcl"
      :defer t
      :init
      ;; TODO calling autoload in (use-package jcl-mode ..) might not be needed
      ;; see autoload docu
      (autoload 'jcl-mode "jcl" nil t))

    (use-package cobol-mode
      ;; :defer t
      :load-path "~/dev/dotfiles/jcl")

    (use-package rexx-mode
      ;; :defer t
      :load-path "~/.emacs.d/rexx-mode")
    :init
    (add-to-list 'auto-mode-alist '("\\.rexx$" . rexx-mode)))

  (use-package eww
    :init
    (defvar-local endless/display-images t)

    (defun endless/toggle-image-display ()
      "Toggle images display on current buffer."
      (interactive)
      (setq endless/display-images
            (null endless/display-images))
      (endless/backup-display-property endless/display-images))


    (defun endless/backup-display-property (invert &optional object)
      "Move the 'display property at POS to 'display-backup.
Only applies if display property is an image.
If INVERT is non-nil, move from 'display-backup to 'display
instead.
Optional OBJECT specifies the string or buffer. Nil means current
buffer."
      (let* ((inhibit-read-only t)
             (from (if invert 'display-backup 'display))
             (to (if invert 'display 'display-backup))
             (pos (point-min))
             left prop)
        (while (and pos (/= pos (point-max)))
          (if (get-text-property pos from object)
              (setq left pos)
            (setq left (next-single-property-change pos from object)))
          (if (or (null left) (= left (point-max)))
              (setq pos nil)
            (setq prop (get-text-property left from object))
            (setq pos (or (next-single-property-change left from object)
                          (point-max)))
            (when (eq (car prop) 'image)
              (add-text-properties left pos (list from nil to prop) object))))))))

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
       ((getenv "isLinuxNew64") 116)
       ((getenv "isLinuxFranzi") 130)
       ((getenv "isLinuxMartinJV") 120)
       ((s-ends-with? "VirtualBox" system-name) 102)
       (t 140)))

    (if (getenv "isLinuxFranzi")
        (display-battery-mode 1))

    (defun find-file-emacs ()
      (interactive)
      (find-file "~/dev/dotfiles/.emacs"))))
