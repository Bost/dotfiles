(define-module (scm-bin gco)
  #:use-module (utils)
  #:export (main gco))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (gco) -s
!#

|#

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
  ((compose
    (partial apply gco)
    (partial apply cdr)
    #;dbg)
   args))