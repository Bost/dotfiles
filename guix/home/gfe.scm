(define-module (gfe)
  #:use-module (utils)
  #:export (main gfe))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (gfe) -s
!#

|#

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
  ((compose
    (partial apply gfe)
    (partial apply cdr)
    #;dbg)
   args))
