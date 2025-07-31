;;; See https://guix.gnu.org/manual/devel/en/html_node/Channels-with-Substitutes.html
(define-module (config channels channel-defs)
  ;; See service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (settings)
  #:use-module (memo)
  #:use-module (guix channels)
  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  ;; simple-service
  #:use-module (gnu home services)
  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; $fish-foreign-env
  ;; #:use-module (gnu packages shells)
  ;; pretty-print
  #:use-module (ice-9 pretty-print)
  #:export (channel-home-service-dwl-guile
            channel-games
            channel-hask-clj
            channel-flat
            channel-rde
            channel-bost
            channel-nonguix
            channel-guixrus
            channel-guix
            channel-guix-past
            syst-channels))

(define m (module-name-for-logging))
(evaluating-module)

(define (create-file-channels-scm additional-channels)
  (let* [(m (format #f "~a [create-file-channels-scm]" m))
         (channels-scm-fullpath (user-dotf "/" channels-scm-relpath))]
    (call-with-values
        (lambda ()
          (let* [(lst-channels-scm
                  ((comp
                    car
                    syntax->datum
                    (partial call-with-input-file channels-scm-fullpath))
                   (read-all read-syntax)))]
            (split-at lst-channels-scm (1- (length lst-channels-scm)))))
      (lambda (prm-fst prm-snd)
        ((comp
          ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
          (lambda (s) (format #t "done\n") s)
          (lambda (sexp)
            (list
             channels-scm-relpath
             #;(scheme-file "channels.scm" (sexp->gexp sexp))
             (local-file
              (let* [(tmpfile (mktmpfile))
                     (port (open-output-file tmpfile))]
                ;; save the channel configuration to a temporary file
                (pretty-print sexp port)
                (close-port port)
                tmpfile)
              channels-scm)))
          (lambda (s)
            (format #t "I ~a Creating ~a ... " m channels-scm-fullpath) s)
          (lambda (sexp) (append prm-fst sexp (list (car prm-snd)))))
         additional-channels)))))
(testsymb 'create-file-channels-scm)

(define* (channel-home-service-dwl-guile #:key (commit #f))
  "Dynamic tiling Wayland compositor configurable in Guile Scheme"
  (let* [(channel-name 'home-service-dwl-guile)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url
       "https://github.com/engstrand-config/home-service-dwl-guile")
      (branch "main")
      (commit commit)
      (introduction
       (make-channel-introduction
        "314453a87634d67e914cfdf51d357638902dd9fe"
        (openpgp-fingerprint
         "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C"))))))

(define* (channel-games #:key (commit #f))
  "https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
Use:
    guix package --load-path=./ --install=factorio
'--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
wrong.
The games channel requires the guix-gaming-channels/games.scm - see above.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'games)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url
       #;"https://gitlab.com/rostislav.svoboda/games"
       #;,(format #f "file://%s/dev/games" home)
       "https://gitlab.com/guix-gaming-channels/games.git")
      (commit commit)
      ;; Enable signature verification:
      (introduction
       (make-channel-introduction
        ;; 1st commit from this repository which can be trusted is:
        "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
        (openpgp-fingerprint
         ;; ... as it was made by some with OpenPGP fingerprint:
         "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))))

(define* (channel-hask-clj #:key (commit #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'hask-clj)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url
       #;"https://github.com/Tass0sm/guix-develop"
       "https://github.com/Bost/haskell-guix"
       #;,(format #f "file://~a/dev/haskell-guix" home))
      (commit commit))))

(define* (channel-flat #:key (commit #f))
  "flatwhatson contains emacs-native-comp, however it doesn't compile.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'flat)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url "https://github.com/flatwhatson/guix-channel.git")
      (commit commit)
      (introduction
       (make-channel-introduction
        "33f86a4b48205c0dc19d7c036c85393f0766f806"
        (openpgp-fingerprint
         "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))))

(define* (channel-rde #:key (commit #f))
  "Andrew Tropin's tools for managing reproducible development environments.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'rde)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url
       "https://git.sr.ht/~abcdw/rde"
       #;,(format #f "file://~a/dev/andrew-rde" home))
      (commit commit)
      (introduction
       (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
         "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

(define* (channel-bost #:key (commit #f))
  "Provides a.o.:
- (bost gnu packages emacs-xyz) module
- clojure, babashka, postgres 13.3, openjdk18
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'bost)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel (name channel-name)
             (url
              ;; "https://github.com/Bost/guix-packages"
              (format #f "file://~a/dev/guix-packages" home))
             (commit commit))))

(define* (channel-nonguix #:key (commit #f))
  "Provides firefox, linux-kernel with non-free proprietary drivers, etc.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'nonguix)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel (name channel-name)
             (url "https://gitlab.com/nonguix/nonguix")
             (commit commit)
;;; Enable signature verification, i.e. declare that in the nonguix repository,
;;; the first commit which can be trusted is the 897c1a470d and it was created
;;; by someone who's GnuPG fingerprint is the "2A39 3FFF ..."
             (introduction
              (make-channel-introduction
               "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
               (openpgp-fingerprint
                "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

(define* (channel-guixrus #:key (commit #f))
  "This channel provides packages and services that are:
* Yet to be merged upstream.
* In alpha or beta stage of development.
* Customized to certain use-cases.
* Nightly releases.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'guixrus)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url "https://git.sr.ht/~whereiseveryone/guixrus")
      (commit commit)
      (introduction
       (make-channel-introduction
        "7c67c3a9f299517bfc4ce8235628657898dd26b2"
        (openpgp-fingerprint
         "CD2D 5EAA A98C CB37 DA91  D6B0 5F58 1664 7F8B E551"))))))

(define* (channel-guix #:key (commit #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  ;; %default-guix-channel
  (let* [(channel-name 'guix)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      ;; Probably either 'branch' or 'commit' can be used.
      ;; branch defaults to "master". See `define-record-type* <channel> ...` in
      ;; guix/channels.scm
      ;; (branch "master")
      (url
       ;; (format #f "file://~a/dev/guix" home)
       ;; "https://git.guix.gnu.org/guix.git" is redirected to codeberg
       "https://codeberg.org/guix/guix.git"
       ;; "https://git.savannah.gnu.org/git/guix.git"
       )
      (commit commit)
      (introduction
       (make-channel-introduction
        "9edb3f66fd807b096b48283debdcddccfea34bad"
        (openpgp-fingerprint
         "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))
(testsymb 'channel-guix)

(define* (channel-guix-past #:key (commit #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'guix-past)]
    (when commit
      (format #t "Channel ~a pinned to ~a\n" channel-name commit))
    (channel
      (name channel-name)
      (url "https://codeberg.org/guix-science/guix-past")
      (commit commit)
;;; The following lines allow 'guix pull' to authenticate this channel. It
;;; requires a recent Guix (July 2020) and can be omitted with older versions.
      (introduction
       (make-channel-introduction
        "0c119db2ea86a389769f4d2b9c6f5c41c027e336"
        (openpgp-fingerprint
         "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5"))))))
(testsymb 'channel-guix-past)

(define* (syst-channels #:key (guix-commit #f) (nonguix-commit #f))
  "Channels needed for the Guix-system configuration
If `guix-commit' and/or `nonguix-commit' are unspecified of #f it means 'use latest
commit(s)'."
  (list
   (channel-guix    #:commit guix-commit)
   (channel-nonguix #:commit nonguix-commit)))
(testsymb 'syst-channels)

(module-evaluated)
