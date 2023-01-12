(define-module (scm-bin chmod)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (chmod) -s
!#

|#

(define (main modifier args)
  ((compose
    exec
    (partial cons* (string-append "chmod +" modifier))
    cdr)
   args))
