(require 's)

(progn
  ;; This works only when bash environment initialised.
  ;; I.e. invoke emacs from CLI or modify emacs24 xfce launcher:
  ;; bash -c -i ~/dev/emacs/src/emacs
  (defun get-font-height () ; font size
    (interactive)
    (cond
     ;; TODO fix get-font-height() for MartinJV
     ((s-ends-with? "new-64" system-name) 116)
     ((s-ends-with? "franzi" system-name) 130)
     ;; (> (getenv "isLinuxMartinJV") 120)
     ((s-ends-with? "VirtualBox" system-name) 102)
     (t 140)))

  (if (s-ends-with? "franzi" system-name)
      (display-battery-mode 1)))

(when (s-ends-with? "VirtualBox" system-name)
  (progn
    (use-package jcl-mode :defer t
      :load-path (concat dotf-dir "/jcl")
      :init
      ;; TODO calling autoload in (use-package jcl-mode ..) might not be needed
      ;; see autoload docu
      (autoload 'jcl-mode "jcl" nil t))

    (use-package cobol-mode ;; :defer t
      :load-path (concat dotf-dir "/jcl"))

    (use-package rexx-mode ;; :defer t
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

(provide 'environment-lib)
