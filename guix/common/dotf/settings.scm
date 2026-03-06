;;; This module is required by #:use-module in some of the scm-bin/*.scm CLI
;;; utilities. The `(format ...)' output will also appear in the console when
;;; such a utility is executed.

(define-module (dotf settings)
  #:use-module (dotf utils)
  ;; #:use-module (ice-9 r5rs)
  #:use-module (srfi srfi-1)    #| remove delete-duplicates |#
)

(define m (module-name-for-logging))
(evaluating-module)

(define-public gitlab "git@gitlab.com:rostislav.svoboda")
(define-public github "git@github.com:Bost")
(define-public user-full-name "Rostislav Svoboda")
(define-public user-mail-address "Rostislav.Svoboda@posteo.net")
(define-public user "bost")
(define-public home (str "/home/" user))
(define-public host-kalus "kalus")
(define-public host-lukas "lukas")
(define-public host-ecke "ecke")
(define-public host-geek "geek")
(define-public host-edge "edge")
(define-public hostnames
  (list host-kalus host-lukas host-ecke host-geek host-edge))

(define-public emacs-init-file "init.el")
(define-public emacs-distros "/.emacs.d.distros")
(define-public home-emacs-distros (str home emacs-distros))

(define (emacs-spacemacs-store-path)
     (let* [(cmd-result-struct
             ((comp
               (lambda (cmd) (exec cmd  #:verbose #f #:return-plist #t))
               cmd->string)
              (list "guix package --profile=/home/bost/.guix-profile --list-installed=emacs-spacemacs\\$")))
            (retcode (plist-get cmd-result-struct #:retcode))]
       (if (zero? retcode)
           ((comp
             ;; (lambda (p) (format #t "~a done\n" f) p)
             ;; (partial filter usb-device?)
             ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
             last
             (lambda (s) (string-split s #\tab))
             car
             )
            (plist-get cmd-result-struct #:results))
           (begin
             ;; error-out
             (error (format #f "~a retcode: ~a\n" m retcode))
             ))))

;; See also:
;;   git-spacemacs
;;   spacemacs-dir
;; files:
;;   guix/home/common/scm-bin/spag.scm
;;   guix/home/common/scm-bin/restore-spacemacs.scm
(define (emacs-profiles-config) ;; branch-kw_to_settings-map
  (list
   ;; (const <profile-name> <profile-configuration>)
   (cons #:develop
         (list (cons #:user-emacs-directory
                     (str home-emacs-distros "/spacemacs/develop/src"))
               (cons #:env
                     (str home-emacs-distros "/spacemacs/develop/cfg"))))
   (cons #:cycle
         (list (cons #:user-emacs-directory
                     (str home-emacs-distros "/spacemacs/cycle/src"))
               (cons #:env
                     (str home-emacs-distros "/spacemacs/cycle/cfg"))))

   (cons #:spguix
         (list (cons #:user-emacs-directory
                     ;; TODO this is a hack. For some reasons this can't be used:
                     ;;   (use-modules (guix scripts package) (guix profiles))
                     ;;   (last (car (list-installed "emacs-spacemacs$" (list %current-profile))))
                     ;; I guess it has something to do with build vs. runtime.
                     ;; The start and stop commands should be implemented in the emacs-spacemacs-wrapped package
                     ;; And also there should be emacs-wrapped and crafted-emacs-wrapped, etc.
                     ;; See also (which-emacs) in guix/home/common/emacs-common.scm
                     (let* [(esss (emacs-spacemacs-store-path))
                            (s (substring esss
                                          (string-length "/gnu/store/")))
                            (checksum (substring
                                       s 0 (- (string-length s)
                                              (string-length "-emacs-spacemacs-1.0-0.<_sha_>"))))
                            (commit (substring
                                     s (+ (string-length checksum)
                                          (string-length "-emacs-spacemacs-1.0-0."))))]
                       (str esss "/share/emacs/site-lisp/spacemacs-1.0-0." commit)))
               (cons #:env
                     (str home-emacs-distros "/spacemacs/spguix/cfg"))))

   (cons #:guix
         (list (cons #:user-emacs-directory
                     (str home-emacs-distros "/spacemacs/guix/src"))
               (cons #:env
                     (str home-emacs-distros "/spacemacs/guix/cfg"))))

   (cons #:crafted
         (list (cons #:user-emacs-directory
                     (str home-emacs-distros "/crafted-emacs"))
               (cons #:env
                     (str home-emacs-distros "/crafted-emacs/personal"))))))
(testsymb 'emacs-profiles-config)

(define-public develop "develop")
(define-public cycle   "cycle")
(define-public spguix  "spguix")
(define-public guix    "guix")
(define-public crafted "crafted")

(define-public profile->branch-kw
  (list
   (cons spguix  #:spguix)
   (cons develop #:develop)
   (cons cycle   #:cycle)
   (cons guix    #:guix)
   (cons crafted #:crafted)))

;; TODO the crafted configuration is not managed by guix home
(define-public spacemacs-profiles
  ((comp
    (partial remove (partial eq? crafted))
    (partial map car))
   profile->branch-kw))

(define-public is-valid-profile?
  (partial string-in? (map car profile->branch-kw)))

(define (get-val profile setting)
  "
(get-val profile #:user-emacs-directory)
(get-val profile #:env)
"
  ;; (format #t "profile ~a; setting ~a\n" profile setting)
  (let* [(branch-kw (cdr (assoc profile profile->branch-kw)))]
    ;; (format #t "branch-kw ~a\n" branch-kw)
    (let* [(settings-map (cdr (assoc branch-kw (emacs-profiles-config))))]
      ;; (format #t "settings-map ~a\n" settings-map)
      (let* [(val (cdr (assoc setting settings-map)))]
        ;; (format #t "profile ~a; setting ~a; val: ~a\n" profile setting val)
        val))))
(testsymb 'get-val)

(define-public (get-src profile)
  "(get-src spacemacs)
;; => \"/home/bost/.emacs.d.distros/spacemacs/develop/src\""
  (get-val profile #:user-emacs-directory))

(define-public (get-cfg profile)
  "(get-cfg spacemacs)
;; => \"/home/bost/.emacs.d.distros/spacemacs/develop/cfg\""
  (get-val profile #:env))

(module-evaluated)
