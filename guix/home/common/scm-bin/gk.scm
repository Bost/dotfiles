(define-module (scm-bin gk)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (gk) -s
!#

|#

(define (main args)
  ((compose
    exec-background
    dbg
    (lambda (p) (append '("gitk") (if (null? p) '("--all") p) '("&")))
    cdr)
   args))
