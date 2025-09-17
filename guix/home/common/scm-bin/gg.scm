(define-module (scm-bin gg)
;;; All used modules must be present in the module (services cli-utils) under:
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

(define m (module-name-for-logging))
(evaluating-module)

(define (main args)
  ((comp
    exec-background
    (partial append (list "git" "gui"))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)
