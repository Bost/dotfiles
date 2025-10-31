(define-module (scm-bin gg)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gg) -s
!#

cd $dotf
./guix/home/common/scm-bin/gg.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define-public (main args)
  ((comp
    exec-background
    (partial append (list "git" "gui"))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)
