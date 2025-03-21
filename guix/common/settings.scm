;;; This module is required by #:use-module in some of the scm-bin/*.scm CLI
;;; utilities. The `(format ...)' output will also appear in the console when
;;; such a utility is executed.

(define-module (settings)
  #:use-module (utils)
  ;; #:use-module (ice-9 r5rs)
  ;; #:use-module (srfi srfi-1)    #| delete-duplicates |#
)

(define m (module-name-for-logging))
(evaluating-module)

(define-public spacemacs "spacemacs")
(define-public spguimacs "spguimacs")
(define-public crafted   "crafted")

(define-public is-valid-profile?
  (partial string-in? (list spacemacs spguimacs crafted)))


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

;; See also:
;;   git-spacemacs
;;   spacemacs-dir
;; files:
;;   guix/home/common/scm-bin/spag.scm
;;   guix/home/common/scm-bin/restore-spacemacs.scm
(define-public emacs-profiles ;; branch-kw_to_settings-map
  (list
   (cons #:develop
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/develop/src")
               (cons #:server-name "develop")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/develop/cfg")))

   (cons #:guix
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/guix/src")
               (cons #:server-name "guix")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/guix/cfg")))

   (cons #:guix-merge
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/guix-merge/src")
               (cons #:server-name "guix-merge")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/guix-merge/cfg")))

   (cons #:shorten-name
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/shorten-name/src")
               (cons #:server-name "shorten-name")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/shorten-name/cfg")))

   (cons #:keyseq
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/spacemacs/keyseq/src")
               (cons #:server-name "keyseq")
               (cons #:env
                     "/home/bost/.emacs.d.distros/spacemacs/keyseq/cfg")))

   (cons #:crafted
         (list (cons #:user-emacs-directory
                     "/home/bost/.emacs.d.distros/crafted-emacs")
               (cons #:server-name "crafted")
               (cons #:env
                     "/home/bost/.emacs.d.distros/crafted-emacs/personal")))))
(testsymb 'emacs-profiles)

;;; Branches in the spguimacs
(define-public origin  "origin")
(define-public develop "develop")
(define-public cycle   "cycle")
(define-public keyseq  "keyseq")
(define-public shorten-name "shorten-name")

(define-public profile->branch-kw
  (list
   ;; (cons spacemacs #:keyseq)
   (cons spacemacs #:shorten-name)
   (cons spguimacs #:guix-merge)
   (cons crafted   #:crafted)))

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

(define-public (get-src    profile) (get-val profile #:user-emacs-directory))
(define-public (get-cfg    profile) (get-val profile #:env))
(define-public (get-server profile) (get-val profile #:server-name))

(define-public spacemacs-dir (get-src spacemacs))

(module-evaluated)
