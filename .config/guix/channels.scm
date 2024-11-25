;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;;; channels added by the home configuration
(define (home-config-channels)
  (list
   ;; TODO the list channels can't be generated, since guix home reconfigure
   ;; expects them to be already available, i.e. guix-pull-ed-in
   (channel (name 'hask-clj)
            (url "https://github.com/Bost/haskell-guix"))

       ;;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
   (channel (name 'bost)
            (url "https://github.com/Bost/guix-packages"))

   (channel (name 'games)
            (url "https://gitlab.com/guix-gaming-channels/games.git")
            (introduction
             (make-channel-introduction
              "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
              (openpgp-fingerprint
              "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))))

(append
 ;; (home-config-channels)

 (cons*
  ;; The flat channel doesn't compile at the moment
  ;; (channel (name 'flat)
  ;;          (url
  ;;           ;; my local copy
  ;;           (string-append "file://" (getenv "HOME") "/dev/flatwatson")
  ;;           #;"https://github.com/flatwhatson/guix-channel.git")
  ;;          (introduction
  ;;           (make-channel-introduction
  ;;            "33f86a4b48205c0dc19d7c036c85393f0766f806"
  ;;            (openpgp-fingerprint
  ;;             "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))

;;; See https://guix.gnu.org/manual/devel/en/html_node/Channels-with-Substitutes.html

  ;; The nonguix channel provides a.o. firefox
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

  %default-channels))
