(define-module (gk)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (gk) -s
!#

|#

(define (main args)
  ((compose
    exec-background
    dbg
    (lambda (p) (append '("gitk") (if (null? p) '("--all") p) '("&")))
    cdr)
   args))