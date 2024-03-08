(define-module (scm-bin gco)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gco))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gco) -s
!#

cd $dotf
./guix/home/common/scm-bin/gco.scm 

|#

(evaluating-module)

(define* (gco #:rest args)
  "Usage:
(gco  \"-f\" \"arg0\")
(gco \"-f arg0\")
(equal? (gco \"-f\" \"arg0\")
        (gco \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "checkout"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply gco)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
