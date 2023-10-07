;;; This module is required by #:use-module in some of the scm-bin/*.scm CLI
;;; utilities. The `(format ...)' output will also appear in the console when
;;; such a utility is executed.

(define-module (settings)
  ;; #:use-module (ice-9 r5rs)
  ;; #:use-module (srfi srfi-1)    #| delete-duplicates |#
  #:export (
            gitlab
            github
            user-full-name
            user-mail-address
            host-lukas
            host-ecke
            host-geek
            hostnames
            user
            home
            spacemacs-dir
            ))

;; (format #t "[settings] evaluating module ...\n")

(define gitlab "git@gitlab.com:rostislav.svoboda")
(define github "git@github.com:Bost")
(define user-full-name "Rostislav Svoboda")
(define user-mail-address "Rostislav.Svoboda@gmail.com")
(define user "bost")
(define home (string-append "/home/" user))

(define spacemacs-dir ".emacs.d.spacemacs")

(define host-lukas "lukas")
(define host-ecke "ecke")
(define host-geek "geek")

(define hostnames (list host-lukas host-ecke host-geek))

;; (format #t "[settings] module evaluated\n")
