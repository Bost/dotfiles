;; (format #t "[system [common settings]] evaluating module ...\n")

(define-module (common settings)
  #:export (user-full-name
            user-mail-address
            ))

(define user-full-name "Rostislav Svoboda")
(define user-mail-address "Rostislav.Svoboda@gmail.com")

(format #t "[system [common settings]] module evaluated\n")
