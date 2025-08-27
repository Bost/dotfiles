(define-module (scm-bin gfe)
;;; All used modules must be present in the module (services scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gfe))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gfe) -s
!#

cd $dotf
./guix/home/common/scm-bin/gfe.scm 

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (gfe #:rest args)
  "Usage:
(gfe \"-f\" \"arg0\")
(gfe \"-f arg0\")
(equal? (gfe \"-f\" \"arg0\")
        (gfe \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "fetch"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply gfe)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
