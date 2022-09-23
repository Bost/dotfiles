(format #t "[common settings] evaluating module...\n")

(define-module (common settings)
  #:export (user-full-name
            user-mail-address
            ))

(define user-full-name "Rostislav Svoboda")
(define user-mail-address "Rostislav.Svoboda@gmail.com")

(format #t "[common settings] module evaluated\n")
