(define-module (scm-bin gg)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gg) -s
!#

cd $dotf
./guix/home/common/scm-bin/gg.scm 

|#

(evaluating-module)

(define (main args)
  ((comp
    exec-background
    dbg
    (lambda (p) (append '("git" "gui") p '("&")))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)
