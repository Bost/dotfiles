(define-module (chmod)
  #:use-module (utils)
  #:export (main))

(define (main modifier args)
  ((compose
    exec
    #;(partial apply system*)
    (partial cons* (string-append "chmod +" modifier))
    cdr)
   args))
