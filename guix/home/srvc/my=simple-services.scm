;; (format #t "[my=simple-services] evaluating module ...\n")
(define-module (srvc my=simple-services)
  ;; See service-file -> with-imported-modules
  #:use-module (common settings)
  #:use-module (utils)
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
            home-dir-config-service
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

;; (format #t "[my=simple-services] home-games-config ~a\n" home-games-config)
;; (format #t "[my=simple-services] home-ecke-config ~a\n" home-ecke-config)

(define extra-channels
  (cond
   [home-ecke-config
    `((channel
       (name 'haskell-and-clojure)
       (url
        ;; "https://github.com/Tass0sm/guix-develop/tassos-guix"
        ;; "https://github.com/Bost/haskell-guix"
        (format #f "file://~a/dev/haskell-guix" home)
        ))
      ;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
      (channel
       (name 'bost)
       (url
        ;; "https://github.com/Bost/guix-packages"
        (format #f "file://~a/dev/guix-packages" home))))]
   [home-games-config
    `(
;;; https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
;;; Use:
;;;     guix package --load-path=./ --install=factorio
;;; '--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
;;; wrong
      (channel
       (name 'guix-gaming-games)
       (url
        ;; "https://gitlab.com/rostislav.svoboda/games"
        ;; (string-append "file://" home "/dev/games")
        "https://gitlab.com/guix-gaming-channels/games.git")
       ;; Enable signature verification:
       (introduction
        (make-channel-introduction
         "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
         (openpgp-fingerprint
          "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F")))))]))
;; (format #t "[my=simple-services] extra-channels: ~a\n" extra-channels)

(define (gaming-configuration extra-channels)
  (list
   (local-dotfile "/" (str (basename xdg-config-home)
                           "/guix-gaming-channels/games.scm"))
   (let* [(lst (car (syntax->datum
                     (call-with-input-file channels-scm-filepath
                       (read-all read-syntax)))))]
     (call-with-values
         (lambda () (split-at lst (1- (length lst))))
       (lambda (fst snd)
         ((compose
           ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
           (lambda (sexp)
             (format #t "########## Creating plain-file gaming-configuration ... \n")
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
;; (format #t "[my=simple-services] gaming-configuration ~a\n" gaming-configuration)

(define (host-ecke-configuration extra-channels)
  (list
   (let* [(lst (car (syntax->datum
                     (call-with-input-file channels-scm-filepath
                       (read-all read-syntax)))))]
     (call-with-values
         (lambda () (split-at lst (1- (length lst))))
       (lambda (fst snd)
         ((compose
           ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
           (lambda (sexp)
             (format #t "\n\n########## Creating plain-file host-ecke-configuration ... \n\n")
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
           (lambda (sexp) (append fst sexp (list (car snd)))))
          extra-channels))))))
;; (format #t "[my=simple-services] host-ecke-configuration ~a\n" host-ecke-configuration)

(define home-dir-config-service
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d directory
  (simple-service
   'home-dir-config-service home-files-service-type
   ((compose
     (partial append
              (cond
               [home-games-config
                (gaming-configuration extra-channels)]
               [home-ecke-config
                (host-ecke-configuration extra-channels)]
               [#t
                (list
                 (local-dotfile "/" channels-scm-filepath))]))
     (partial append
              (remove
               unspecified-or-empty-or-false?
               (list
                (local-dotfile "/" ".guile") ;; used by `guix repl'
                (local-dotfile "/" ".gitconfig")
                (local-dotfile "/" ".spacemacs")
                (local-dotfile "/" ".spguimacs")
                (local-dotfile "/guix/home/" "local-stuff.fish"))))
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
                                #:recursive? #t)))
               (let ((destination
                      (str ".emacs.d"
                           "/private/themes"
                           "/farmhouse-light-mod-theme"))
                     (dir (str ".emacs.d/private/local"
                               "/farmhouse-light-mod-theme")))
                 `(,destination ,(local-file (dotfiles-home "/" dir)
                                             #:recursive? #t)))
;;; See value of `spacemacs-data-directory' in the $dev/guix-packages/spacemacs
               (let ((destination
                      (str ".local/share/spacemacs"
                           "/private/themes"
                           "/farmhouse-light-mod-theme"))
                     (dir (str ".emacs.d/private/local"
                               "/farmhouse-light-mod-theme")))
                 `(,destination ,(local-file (dotfiles-home "/" dir)
                                             #:recursive? #t)))))))))
;; (format #t "[my=simple-services] home-dir-config-service: ~a\n" home-dir-config-service)

(format #t "[my=simple-services] module evaluated\n")
