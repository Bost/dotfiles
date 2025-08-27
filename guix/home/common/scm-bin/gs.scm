(define-module (scm-bin gs)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gs))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gs) -s
!#

cd $dotf
./guix/home/common/scm-bin/gs.scm 

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (gs #:rest args)
  "Usage:
(gs \"-f\" \"arg0\")
(gs \"-f arg0\")
(equal? (gs \"-f\" \"arg0\")
        (gs \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "status" "--short" "--branch"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply gs)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
