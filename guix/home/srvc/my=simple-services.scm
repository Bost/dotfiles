;; (define m "[my=simple-services]") ;; module-name
;; (format #t "~a evaluating module ...\n" m)

(define-module (srvc my=simple-services)
  ;; See service-file -> with-imported-modules
  #:use-module ((common settings) #:prefix ss:)
  #:use-module ((utils) #:prefix su:)
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
            extra-channels
            gaming-configuration
            host-ecke-configuration
            home-dir-cfg-srvc
            ))

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

(define extra-channels
  (cond
   [(su:home-ecke-config)
    `((channel (name 'haskell-and-clojure)
               (url
                ;; "https://github.com/Tass0sm/guix-develop/tassos-guix"
                ;; "https://github.com/Bost/haskell-guix"
                ,(format #f "file://~a/dev/haskell-guix" su:home)
                ))
      ;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
      (channel (name 'bost)
               (url
                ;; "https://github.com/Bost/guix-packages"
                ,(format #f "file://~a/dev/guix-packages" su:home))))]
   [(su:home-geek-config)
    `((channel (name 'haskell-and-clojure)
               (url
                ;; "https://github.com/Tass0sm/guix-develop/tassos-guix"
                "https://github.com/Bost/haskell-guix"
                ;; ,(format #f "file://~a/dev/haskell-guix" su:home)
                ))
      ;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
      (channel (name 'bost)
               (url
                "https://github.com/Bost/guix-packages"
                ;; ,(format #f "file://~a/dev/guix-packages" su:home)
                )))]
   [(su:home-games-config)
    (begin
      ;; (format #t "~a cond su:home-games-config\n" m)
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
                    ))))))]))

(define (gaming-configuration extra-channels)
  (list
   (local-dotfile "/" (su:str (basename xdg-config-home)
                              "/guix-gaming-channels/games.scm"))
   (let* [(lst (car (syntax->datum
                     (call-with-input-file channels-scm-filepath
                       (su:read-all read-syntax)))))]
     (call-with-values
         (lambda () (split-at lst (1- (length lst))))
       (lambda (fst snd)
         ((compose
           ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
           (lambda (sexp)
             (format #t "~a ### Creating plain-file gaming-configuration ... \n" m)
             (list
              channels-scm-filepath
              #;(scheme-file "channels.scm" (sexp->gexp sexp))
              (local-file
               (let* [(tmpfile (tmpnam))
                      (port
                       ;; create temporary file
                       ;; (mkstemp! (string-copy "/tmp/myfile-XXXXXX"))
                       (open-output-file tmpfile))]
                 ;; save the channel configuration to a temporary file
                 (pretty-print sexp port)
                 (close-port port)
                 tmpfile)
               "channels.scm")))
           (lambda (sexp) (append fst sexp (list (car snd)))))
          extra-channels))))))
;; (format #t "~a gaming-configuration ~a\n" m gaming-configuration)

(define (host-ecke-configuration extra-channels)
  ;; (format #t "~a [host-ecke-configuration] extra-channels: ~a\n" m extra-channels)
  ;; (format #t "~a [host-ecke-configuration] channels-scm-filepath: ~a\n" m channels-scm-filepath)
  ;; (format #t "~a [host-ecke-configuration] (dotfiles-home \"/\" channels-scm-filepath): ~a\n" m (dotfiles-home "/" channels-scm-filepath))
  (list
   (let* [(channels-scm (dotfiles-home "/" channels-scm-filepath))
          (lst (car (syntax->datum
                     (call-with-input-file channels-scm
                       (su:read-all read-syntax)))))]
     ;; (format #t "~a [host-ecke-configuration] lst: ~a\n" m lst)
     (call-with-values
         (lambda () (split-at lst (1- (length lst))))
       (lambda (fst snd)
         ((compose
           ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
           (lambda (sexp)
             ;; (format #t "~a ### Creating plain-file host-ecke-configuration ... \n" m)
             (let* [(ret
                     (list
                      channels-scm-filepath
                      ;; (scheme-file "channels.scm" (sexp->gexp sexp))
                      (local-file
                       (let* [(tmpfile (tmpnam))
                              (port
                               ;; create temporary file
                               #;(mkstemp! (string-copy "/tmp/myfile-XXXXXX"))
                               (open-output-file tmpfile))]
                         ;; save the channel configuration to a temporary file
                         (pretty-print sexp port)
                         (close-port port)
                         tmpfile)
                       "channels.scm")))]
               ret))
           ;; (lambda (p) (format #t "$$$$$$$$$$$$$$ 1.\n") p)
           (lambda (sexp) (append fst sexp (list (car snd))))
           ;; (lambda (p) (format #t "$$$$$$$$$$$$$$ 0.\n") p)
           )
          extra-channels))))))
;; (format #t "~a host-ecke-configuration ~a\n" m host-ecke-configuration)

(define home-dir-cfg-srvc-files
  ((compose
    ;; (lambda (p) (format #t "$$$$$$$$$$$$$$ 3.\n") p)
    (su:partial append
                (cond
                 [(su:home-games-config)
                  (gaming-configuration extra-channels)]
                 [(su:home-ecke-config)
                  ;; (format #t "~a cond su:home-ecke-config\n" m)
                  (let* [(ret (host-ecke-configuration extra-channels))]
                    ;; (format #t "~a cond su:home-ecke-config; ret:\n\n~a\n\n" m ret)
                    ret
                    )]
                 [#t
                  (list
                   (local-dotfile "/" channels-scm-filepath))]))
    ;; (lambda (p) (format #t "$$$$$$$$$$$$$$ 2.\n") p)
    (su:partial append
                (remove
                 su:unspecified-or-empty-or-false?
                 (list
                  (local-dotfile "/" ".guile") ;; used by `guix repl'
                  (local-dotfile "/" ".gitconfig")
                  (local-dotfile "/" ".spacemacs")
                  (local-dotfile "/" ".spguimacs")
                  (local-dotfile "/guix/home/" "local-stuff.fish"))))
    ;; (lambda (p) (format #t "$$$$$$$$$$$$$$ 1.\n") p)
    (su:partial append
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
                                  #:recursive? #t)))
                 (let ((destination
                        (su:str ".emacs.d"
                                "/private/themes"
                                "/farmhouse-light-mod-theme"))
                       (dir (su:str ".emacs.d/private/local"
                                    "/farmhouse-light-mod-theme")))
                   `(,destination ,(local-file (dotfiles-home "/" dir)
                                               #:recursive? #t)))
;;; See value of `spacemacs-data-directory' in the $dev/guix-packages/spacemacs
                 (let ((destination
                        (su:str ".local/share/spacemacs"
                                "/private/themes"
                                "/farmhouse-light-mod-theme"))
                       (dir (su:str ".emacs.d/private/local"
                                    "/farmhouse-light-mod-theme")))
                   `(,destination ,(local-file (dotfiles-home "/" dir)
                                               #:recursive? #t)))))
    ;; (lambda (p) (format #t "$$$$$$$$$$$$$$ 0.\n") p)
    )
   ;; empty list
   (list)))
;; (format #t "~a home-dir-cfg-srvc-files: ~a\n" m home-dir-cfg-srvc-files)
;; (format #t "~a (defined? 'home-dir-cfg-srvc-files): ~a\n" m (defined? 'home-dir-cfg-srvc-files))

(define home-dir-cfg-srvc
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d directory
  (simple-service
   'home-dir-cfg-srvc home-files-service-type home-dir-cfg-srvc-files))
;; (format #t "~a (defined? 'home-dir-cfg-srvc:) ~a\n" m (defined? 'home-dir-cfg-srvc))

(format #t "~a module evaluated\n" m)
