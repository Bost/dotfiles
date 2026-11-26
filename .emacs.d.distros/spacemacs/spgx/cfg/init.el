;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defvar my-spacemacs-profile)
(defvar my-spacemacs-leading-themes)
(setq my-spacemacs-profile "spgx"
      my-spacemacs-leading-themes '(spacemacs-light spacemacs-dark))

;; Editable init files can be symlinks into the checkout. Guix Home installs
;; the shared file alongside the profile directories in the home directory.
(let ((source (expand-file-name "../../init-common.el"
                                (file-name-directory
                                 (file-truename load-file-name)))))
  (load (if (file-readable-p source)
            source
          (expand-file-name "../../init-common.el"
                            (file-name-directory load-file-name)))
        nil nil t))
