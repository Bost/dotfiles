(define-module (srvc home-dir-cfg)
  ;; See service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (memo)
  #:use-module (fs-utils)
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
  #:export (
            home-dir-cfg-srvc
            ))

(define m (module-name-for-logging))

;; ;; See https://10years.guix.gnu.org/static/slides/05-wilson.org
;; (define (home-xsettingsd-files-service config)
;;   (list `(".config/xsettingsd/xsettingsd.conf"
;;           ,(local-file "xsettingsd.conf"))))

;; (define home-xsettingsd-service-type
;;   (service-type (name 'home-xsettingsd)
;;                 (extensions
;;                  (list (service-extension
;;                         home-files-service-type
;;                         home-xsettingsd-files-service)))
;;                 (default-value #f)
;;                 (description "Configures UI appearance settings for Xorg
;; sessions using the xsettingsd daemon.")))

(define (extra-channels use-remote-url)
  "use-remote-url - if #t then use github or gitlab, etc."
  (cond
   [(home-lukas-config)
    `(
      ;; provides:
      ;; - (bost packages emacs-xyz) module
      ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
      (channel (name 'bost)
               (url
                ,(if use-remote-url
                     "https://github.com/Bost/guix-packages"
                     (format #f "file://~a/dev/guix-packages" home))))
      )]
   [(home-ecke-config)
    `((channel (name 'haskell-and-clojure)
               (url
                ,(if use-remote-url
                     ;; "https://github.com/Tass0sm/guix-develop/tassos-guix"
                     "https://github.com/Bost/haskell-guix"
                     (format #f "file://~a/dev/haskell-guix" home))))

      ;; provides:
      ;; - (bost packages emacs-xyz) module
      ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
      (channel (name 'bost)
               (url
                ,(if use-remote-url
                     "https://github.com/Bost/guix-packages"
                     (format #f "file://~a/dev/guix-packages" home))))

      ;; Andrew Tropin's tools for managing reproducible development
      ;; environments
      (channel (name 'rde)
               (url "https://git.sr.ht/~abcdw/rde")
               (introduction
                (make-channel-introduction
                 "257cebd587b66e4d865b3537a9a88cccd7107c95"
                 (openpgp-fingerprint
                  "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      )]
   [(home-geek-config)
    `((channel (name 'haskell-and-clojure)
               (url
                ,(if use-remote-url
                     ;; "https://github.com/Tass0sm/guix-develop/tassos-guix"
                     "https://github.com/Bost/haskell-guix"
                     (format #f "file://~a/dev/haskell-guix" home))))
      ;; provides:
      ;; - (bost packages emacs-xyz) module
      ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
      (channel (name 'bost)
               (url
                ,(if use-remote-url
                     "https://github.com/Bost/guix-packages"
                     (format #f "file://~a/dev/guix-packages" home)))))]
   [(home-games-config)
    `(
;;; https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
;;; Use:
;;;     guix package --load-path=./ --install=factorio
;;; '--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
;;; wrong
      (channel (name 'guix-gaming-games)
               (url
                ;; "https://gitlab.com/rostislav.svoboda/games"
                ;; ,(format #f "file://%s/dev/games" home)
                "https://gitlab.com/guix-gaming-channels/games.git")
               ;; Enable signature verification:
               (introduction
                (make-channel-introduction
                 ;; The 1st commit from this repository which can be trusted is:
                 "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
                 (openpgp-fingerprint
                  ;; ... as it was made by some with OpenPGP fingerprint:
                  "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"
                  )))))]))
(testsymb 'extra-channels)

(define (create-channels-scm additional-channels)
  (let* [(channels-scm (dotfiles-home "/" channels-scm-filepath))
         (lst
          ((compose
            car
            syntax->datum
            (partial call-with-input-file channels-scm))
           (read-all read-syntax)))]
    (call-with-values
        (lambda () (split-at lst (1- (length lst))))
      (lambda (fst snd)
        ((compose
          ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
          (lambda (s) (format #t "done\n") s)
          (lambda (sexp)
            (list
             channels-scm-filepath
             #;(scheme-file "channels.scm" (sexp->gexp sexp))
             (local-file
              (let* [(tmpfile (mktmpfile))
                     (port (open-output-file tmpfile))]
                ;; save the channel configuration to a temporary file
                (pretty-print sexp port)
                (close-port port)
                tmpfile)
              "channels.scm")))
          (lambda (s) (format #t "~a ### Creating channels.scm ... " m) s)
          (lambda (sexp) (append fst sexp (list (car snd)))))
         additional-channels)))))
(testsymb 'create-channels-scm)

(define home-dir-cfg-srvc-files
  ((compose
    ;; (lambda (p) (format #t "############## 3.\n") p)
    (partial append
             (cond
              [(is-system-ecke)
               (list
                (create-channels-scm
                 `(
                   (channel (name 'haskell-and-clojure)
                            (url
                             ;; "https://github.com/Tass0sm/guix-develop/tassos-guix"
                             ;; "https://github.com/Bost/haskell-guix"
                             ,(format #f "file://~a/dev/haskell-guix" home)
                             ))

                   ;; provides:
                   ;; - (bost packages emacs-xyz) module
                   ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
                   (channel (name 'bost)
                            (url
                             ;; "https://github.com/Bost/guix-packages"
                             ,(format #f "file://~a/dev/guix-packages" home)))

                   #|
;;; Andrew Tropin's tools for managing reproducible development environments
                   (channel (name 'rde)
                            (url "https://git.sr.ht/~abcdw/rde")
                            (introduction
                             (make-channel-introduction
                              "257cebd587b66e4d865b3537a9a88cccd7107c95"
                              (openpgp-fingerprint
                               "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
                   |#
                   #|
;;; https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
;;; Use:
;;;     guix package --load-path=./ --install=factorio
;;; '--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
;;; wrong
                   ;; requires the guix-gaming-channels/games.scm - see below
                   (channel (name 'guix-gaming-games)
                            (url
                             ;; "https://gitlab.com/rostislav.svoboda/games"
                             ;; ,(format #f "file://%s/dev/games" home)
                             "https://gitlab.com/guix-gaming-channels/games.git")
                            ;; Enable signature verification:
                            (introduction
                             (make-channel-introduction
                              ;; The 1st commit from this repository which can be trusted is:
                              "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
                              (openpgp-fingerprint
                               ;; ... as it was made by some with OpenPGP fingerprint:
                               "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"
                               ))))
                   |#
                   ))
                #|
                (local-dotfile "/" (str (basename xdg-config-home)
                                        "/guix-gaming-channels/games.scm"))
                |#
                )]
              [(is-system-geek)
               (list
                (create-channels-scm
                 `(
                   ;; provides:
                   ;; - (bost packages emacs-xyz) module
                   ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
                   (channel (name 'bost)
                            (url
                             ;; "https://github.com/Bost/guix-packages"
                             ,(format #f "file://~a/dev/guix-packages" home))))))]
              [(is-system-lukas)
               (list
                (create-channels-scm
                 `(
                   ;; provides:
                   ;; - (bost packages emacs-xyz) module
                   ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
                   (channel (name 'bost)
                            (url
                             "https://github.com/Bost/guix-packages"
                             ;; ,(format #f "file://~a/dev/guix-packages" home)
                             )))))]
              [#t
               (list
                (local-dotfile "/" channels-scm-filepath))]))
    ;; (lambda (p) (format #t "############## 2.\n") p)
    (partial append
             (remove
              unspecified-or-empty-or-false?
              (list
               (local-dotfile "/" ".guile") ;; used by `guix repl'
               (local-dotfile "/" ".gitconfig")
               (local-dotfile "/" ".spacemacs")
               (local-dotfile "/" ".spguimacs")
               (local-dotfile "/guix/home/" "local-stuff.fish"))))
    ;; (lambda (p) (format #t "############## 1.\n") p)
    (partial append
;;; This can't be used:
;;;           `((".emacs.d/private" ;; destination
;;;              ,(local-file (dotfiles-home "/.emacs.d/private")
;;;                           #:recursive? #t)))
;;; because:
;;; 1. Can't store the whole ".emacs.d/private" since there are some README.md
;;; files and `git ... rebase develop cycle' b/c they will be symlinked (from
;;; the /gnu/store/).
;;;
;;; 2. Can't store the ".emacs.d/private" w/o the README.md files and restore
;;; them after `guix home ...', since `git restore ...' overwrites the symlink
;;; (to the /gnu/store/).
             (list
              (let ((dir "bin"))
                `(,dir ;; destination
                  ,(local-file (dotfiles-home "/" dir)
                               #:recursive? #t)))))
    ;; (lambda (p) (format #t "############## 0.\n") p)
    )
   ;; empty list
   (list)))
(testsymb 'home-dir-cfg-srvc-files)

(define home-dir-cfg-srvc
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d directory
  (simple-service
   'home-dir-cfg-srvc home-files-service-type home-dir-cfg-srvc-files))
(testsymb 'home-dir-cfg-srvc)
;; (format #t "~a (defined? 'home-dir-cfg-srvc:) ~a\n" m (defined? 'home-dir-cfg-srvc))

(define (repl)
  (use-modules (srvc home-dir-cfg))
  (load (string-append (getenv "dotf") "/guix/home/srvc/home-dir-cfg.scm"))
  )
