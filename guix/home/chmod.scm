(define-module (chmod)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (chmod) -s
!#

|#

(define (main modifier args)
  ((compose
    exec
    (partial cons* (string-append "chmod +" modifier))
    cdr)
   args))
