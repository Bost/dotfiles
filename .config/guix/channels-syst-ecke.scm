;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;;; The list channels can't be generated, since guix home reconfigure
;;; expects them to be already available, i.e. guix-pull-ed-in
(cons*
;;; See https://guix.gnu.org/manual/devel/en/html_node/Channels-with-Substitutes.html

 ;; Provides firefox, linux-kerner with non-free proprietary drivers, etc.
 (channel (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix")

;;; Enable signature verification, i.e. declare that in the nonguix repository,
;;; the first commit which can be trusted is the 897c1a470d and it was created
;;; by someone who's GnuPG fingerprint is the "2A39 3FFF ..."
          (introduction
           (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 %default-channels)
