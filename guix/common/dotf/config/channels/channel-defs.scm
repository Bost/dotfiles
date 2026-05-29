;;; See https://guix.gnu.org/manual/devel/en/html_node/Channels-with-Substitutes.html
(define-module (dotf config channels channel-defs)
  ;; See service-file -> with-imported-modules
  #:use-module (dotf utils)
  #:use-module (dotf settings)
  #:use-module (dotf memo)
  #:use-module (guix channels)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)        ; list-processing procedures
  ;; $fish-foreign-env
  ;; #:use-module (gnu packages shells)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 optargs)      ; define*-public
  )

(define m (module-name-for-logging))
(evaluating-module)

(define dev (getenv "dev"))

(define* (get-url repo-name #:key (use-local-checkout #f))
  (str (if use-local-checkout
           (str "file://" dev)
           "https://codeberg.org/Bost/")
       "/" repo-name))

(define (create-file-channels-scm additional-channels)
  (let* [(channels-scm-fullpath (user-dotf "/" channels-scm-relpath))]
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
             ;; (scheme-file "channels.scm" (sexp->gexp sexp))
             (local-file
              (let* [(tmpfile (mktmpfile))
                     (port (open-output-file tmpfile))]
                ;; save the channel configuration to a temporary file
                (pretty-print sexp port)
                (close-port port)
                tmpfile)
              channels-scm)))
          (lambda (s)
            (format #t "I ~a Creating ~a ... " f channels-scm-fullpath) s)
          (lambda (sexp) (append prm-fst sexp (list (car prm-snd)))))
         additional-channels)))))
(testsymb 'create-file-channels-scm)

(def*-public (channel-home-service-dwl-guile #:key (commit #f))
  "Dynamic tiling Wayland compositor configurable in Guile Scheme"
  (let* [(channel-name 'home-service-dwl-guile)]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
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
(testsymb 'channel-home-service-dwl-guile)

(def*-public (channel-games #:key (commit #f) (use-local-checkout #f))
  "https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
Use:
    guix package --load-path=./ --install=factorio
'--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
wrong.
The games channel requires the guix-gaming-channels/games.scm - see above.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'games)
         ;; (url (format "file://~a/games" dev))
         ;; (url "https://gitlab.com/rostislav.svoboda/games")
         (url "https://gitlab.com/guix-gaming-channels/games.git")]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit)
     ;; Enable signature verification:
     (introduction
      (make-channel-introduction
       ;; 1st commit from this repository which can be trusted is:
       "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
       (openpgp-fingerprint
        ;; ... as it was made by some with OpenPGP fingerprint:
        "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))))
(testsymb 'channel-games)

(def*-public (channel-hask-clj #:key (commit #f) (use-local-checkout #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'hask-clj)
         (url (get-url "haskell-guix"
                       #:use-local-checkout use-local-checkout))]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit))))
(testsymb 'channel-hask-clj)

(def*-public (channel-flat #:key (commit #f))
  "flatwhatson contains emacs-native-comp, however it doesn't compile.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed.

TODO make-dist script contains Guile code for package-export. See
https://github.com/flatwhatson/guix-channel/blob/master/scripts/make-dist"
  (let* [(channel-name 'flat)]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url "https://github.com/flatwhatson/guix-channel.git")
     (commit commit)
     (introduction
      (make-channel-introduction
       "33f86a4b48205c0dc19d7c036c85393f0766f806"
       (openpgp-fingerprint
        "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))))
(testsymb 'channel-flat)

(def*-public (channel-rde #:key (commit #f))
  "Andrew Tropin's tools for managing reproducible development environments.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'rde)
         (url "https://git.sr.ht/~abcdw/rde")
         ;; (url (format #f "file://~a/andrew-rde" dev))
         ]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit)
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))
(testsymb 'channel-rde)

(def*-public (channel-bost #:key (commit #f) (use-local-checkout #f))
  "Provides a.o.:
- (bost gnu packages emacs-xyz) module
- clojure, babashka, postgres 13.3, openjdk18
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'bost)
         (url (get-url "guix-packages"
                       #:use-local-checkout use-local-checkout))]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit))))
(testsymb 'channel-bost)

(def*-public (channel-nonguix #:key (commit #f) (use-local-checkout #f))
  "Provides firefox, linux-kernel with non-free proprietary drivers, etc.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'nonguix)
         ;; (url (format #f "file://~a/nonguix" dev))
         (url "https://gitlab.com/nonguix/nonguix")]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit)
;;; Enable signature verification, i.e. declare that in the nonguix repository,
;;; the first commit which can be trusted is the 897c1a470d and it was created
;;; by someone who's GnuPG fingerprint is the "2A39 3FFF ..."
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))
(testsymb 'channel-nonguix)

(def*-public (channel-guixrus #:key (commit #f) (use-local-checkout #f))
  "This channel provides packages and services that are:
* Yet to be merged upstream.
* In alpha or beta stage of development.
* Customized to certain use-cases.
* Nightly releases.
Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'guixrus)
         ;; (url "https://git.sr.ht/~whereiseveryone/guixrus")
         (url (get-url (str channel-name)
                       #:use-local-checkout use-local-checkout))]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit))

    ;; (channel
    ;;   (name channel-name)
    ;;   (url "https://git.sr.ht/~whereiseveryone/guixrus")
    ;;   (commit commit)
    ;;   (introduction
    ;;    (make-channel-introduction
    ;;     "7c67c3a9f299517bfc4ce8235628657898dd26b2"
    ;;     (openpgp-fingerprint
    ;;      "CD2D 5EAA A98C CB37 DA91  D6B0 5F58 1664 7F8B E551"))))
    ))
(testsymb 'channel-guixrus)

(def*-public (channel-guix #:key (commit #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  ;; %default-guix-channel
  (let* [(channel-name 'guix)
         (url
          ;; (format #f "file://~a/guix" dev)
          ;; "https://git.guix.gnu.org/guix.git" is redirected to codeberg
          "https://codeberg.org/guix/guix.git"
          ;; "https://git.savannah.gnu.org/git/guix.git"
          )
         ]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
      (name channel-name)
      ;; Probably either 'branch' or 'commit' can be used.
      ;; branch defaults to "master". See `define-record-type* <channel> ...` in
      ;; guix/channels.scm
      ;; (branch "master")
      (url url)
      (commit commit)
      (introduction
       (make-channel-introduction
        "9edb3f66fd807b096b48283debdcddccfea34bad"
        (openpgp-fingerprint
         "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))
(testsymb 'channel-guix)

(def*-public (channel-guix-past #:key (commit #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'guix-past)]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
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

(def*-public (channel-guix-android #:key (commit #f))
  "Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'guix-android)]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
   (channel
    (name channel-name)
    (url "https://framagit.org/tyreunom/guix-android.git")
    (commit commit)
    (introduction
     (make-channel-introduction
      "d031d039b1e5473b030fa0f272f693b469d0ac0e"
      (openpgp-fingerprint
       "1EFB 0909 1F17 D28C CBF9  B13A 53D4 57B2 D636 EE82"))))))
(testsymb 'channel-guix-android)

(def*-public (channel-guix-science #:key (commit #f))
  "Provides binary substitutes for x86_64-linux, which may no longer be available
in the nonguix channel, like e.g. for firefox

Pin to a specific commit instead of pulling-in the lastest so that this
channel doesn't get rebuild everytime `guix pull ...` is executed."
  (let* [(channel-name 'guix-science)]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
      (name channel-name)
      (url "https://codeberg.org/guix-science/guix-science.git")
      (commit commit)
      (introduction
       (make-channel-introduction
        "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
        (openpgp-fingerprint
         "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))))
(testsymb 'channel-guix-science)

(def*-public (channel-guix-guake #:key (commit #f) (use-local-checkout #f))
  "Provides a.o.:
- (bost gnu packages guake) module
- (bost gnu packages gtk) module"
  (let* [(channel-name 'guix-guake)
         (url (get-url (str channel-name)
                       #:use-local-checkout use-local-checkout))]
    (when commit
      (my=warn "~a Channel ~a pinned to ~a\n" f channel-name commit))
    (channel
     (name channel-name)
     (url url)
     (commit commit))))
(testsymb 'channel-guix-guake)

(module-evaluated)
