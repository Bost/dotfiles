(define-module (gg)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (gg) -s
!#

|#

(define (main args)
  ((compose
    exec-background
    dbg
    (lambda (p) (append '("git" "gui") p '("&")))
    cdr)
   args))
