(define-module (scm-bin gk)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gk) -s
!#

cd $dotf
./guix/home/common/scm-bin/gk.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (main args)
  ((comp
    exec-background
    dbg
    (lambda (p) (append '("gitk") (if (null? p) '("--all") p)))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)
