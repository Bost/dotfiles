;;; This module is required by #:use-module in some of the scm-bin/*.scm CLI
;;; utilities. The `(format ...)' output will also appear in the console when
;;; such a utility is executed.

;; (format #t "[<home> common settings] evaluating module ...\n")
(define-module (common settings)
  ;; #:use-module (ice-9 r5rs)
  ;; #:use-module (srfi srfi-1)    #| delete-duplicates |#
  #:export (user-full-name
            user-mail-address
            host-lukas
            host-ecke
            host-geek
            hostnames
            ))

(define user-full-name "Rostislav Svoboda")
(define user-mail-address "Rostislav.Svoboda@gmail.com")

(define host-lukas "lukas")
(define host-ecke "ecke")
(define host-geek "geek")

(define hostnames (list host-lukas host-ecke host-geek))

;; (format #t "[<home> common settings] module evaluated\n")
