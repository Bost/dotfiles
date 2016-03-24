;; (require 's) ; takes about 100 to load!

;; This works only when bash environment initialised.
;; I.e. invoke emacs from CLI or modify emacs24 xfce launcher:
;; bash -c -i ~/dev/emacs/src/emacs

(cond
 ;; (s-ends-with?  system-name) ; s-ends-with? needs (require 's)
 ((string= "bost-new-64" system-name) (setq is-new-64 t))
 ((string= "franzi" system-name)      (setq is-franzi t))
 ((string= "martin-jv" system-name)   (setq is-martin-jv t))
 ((string= "VirtualBox" (substring system-name 9 19)) (setq is-virt-box t)))

(defun get-font-height () ; font size
  (interactive)
  (cond
   ;; TODO fix get-font-height() for MartinJV
   ((boundp 'is-new-64) 116)
   ((boundp 'is-franzi) 130)
   ((boundp 'is-martin-jv) 122)
   ((boundp 'is-virt-box) 102)
   (t 140)))

(if (boundp 'is-franzi)
    (display-battery-mode 1))

(use-package jcl-mode :defer t
  :if (boundp 'is-virt-box)
  :load-path (dotf-dir "/jcl") ; auto concatenation
  :config
  ;; TODO calling autoload in (use-package jcl-mode ..) might not be needed
  ;; see autoload docu
  (autoload 'jcl-mode "jcl" nil t))

(use-package cobol-mode ;; :defer t
  :if (boundp 'is-virt-box)
  :load-path (dotf-dir "/jcl"))

(use-package rexx-mode ;; :defer t
  :if (boundp 'is-virt-box)
  :load-path "~/.emacs.d/rexx-mode"
  :config
  (add-to-list 'auto-mode-alist '("\\.rexx$" . rexx-mode)))

(if (boundp 'is-virt-box)
    (use-package eww :defer t :ensure t
      :config
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
                (add-text-properties left pos (list from nil to prop) object)))))))

  ;;;;;;;;;;;;;;;;;;;;;;; else: non-virt-box machines

  (use-package paradox :ensure t
    :bind (("<f9>"   . paradox-list-packages) ; TODO auto enable/disable evil-mode
           ("<s-f9>" . paradox-upgrade-packages))
    :init
    (defun package-auto-upgrade ()
      (interactive)
      (package-list-packages)
      (package-menu-mark-obsolete-for-deletion)
      (package-menu-mark-upgrades)
      (package-menu-execute))

    (use-package spinner :defer t :ensure t)
    (setq paradox-github-token (getenv "GITHUB_TOKEN")
          paradox-automatically-star t)))

(provide 'environment-lib)
