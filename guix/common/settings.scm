;;; This module is required by #:use-module in some of the scm-bin/*.scm CLI
;;; utilities. The `(format ...)' output will also appear in the console when
;;; such a utility is executed.

(define-module (settings)
  #:use-module (utils)
  ;; #:use-module (ice-9 r5rs)
  #:use-module (srfi srfi-1)    #| remove delete-duplicates |#
)

(define m (module-name-for-logging))
(evaluating-module)

(define-public gitlab "git@gitlab.com:rostislav.svoboda")
(define-public github "git@github.com:Bost")
(define-public user-full-name "Rostislav Svoboda")
(define-public user-mail-address "Rostislav.Svoboda@gmail.com")
(define-public user "bost")
(define-public home (str "/home/" user))
(define-public host-lukas "lukas")
(define-public host-ecke "ecke")
(define-public host-geek "geek")
(define-public host-edge "edge")
(define-public hostnames (list host-lukas host-ecke host-geek host-edge))

(define-public emacs-init-file "init.el")

;; See also:
;;   git-spacemacs
;;   spacemacs-dir
;; files:
;;   guix/home/common/scm-bin/spag.scm
;;   guix/home/common/scm-bin/restore-spacemacs.scm
(define-public emacs-profiles ;; branch-kw_to_settings-map
  (list
   ;; ~/dev/dotfiles/.emacs-profiles.el
   ;; (const <profile-name> <profile-configuration>)
   (cons #:develop
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/develop/src")
               (cons #:server-name "develop")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/develop/cfg")))
   (cons #:guix-merge
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/guix-merge/src")
               (cons #:server-name "guix-merge")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/guix-merge/cfg")))

   (cons #:crafted
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/crafted-emacs")
               (cons #:server-name "crafted")
               (cons #:env
                     "/home/bost/.emacs.d.distros/crafted-emacs/personal")))))
(testsymb 'emacs-profiles)

;;; Branches in the spguimacs
(define-public develop    "develop")
(define-public guix-merge "guix-merge")

;; emacs-profiles
(define-public develop   "develop")
(define-public spguimacs "spguimacs")
(define-public crafted   "crafted")

(define-public profile->branch-kw
  (list
   (cons develop   #:develop)
   (cons spguimacs #:guix-merge)
   (cons crafted   #:crafted)))

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
(get-val profile #:server-name)
"
  ;; (format #t "profile ~a; setting ~a\n" profile setting)
  (let* [(branch-kw (cdr (assoc profile profile->branch-kw)))]
    ;; (format #t "branch-kw ~a\n" branch-kw)
    (let* [(settings-map (cdr (assoc branch-kw emacs-profiles)))]
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

(define-public (get-server profile)
  "(get-server spacemacs) ;; => \"develop\""
  (get-val profile #:server-name))

(define-public spacemacs-dir (get-src spguimacs))

(module-evaluated)
