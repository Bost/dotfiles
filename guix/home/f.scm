(define-module (f)
  #:use-module (utils)
  #:export (main f))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (f) -s
!#

|#

(define* (f #:rest args)
  "Usage:
(f  \"-f\" \"arg0\")
(f \"-f arg0\")
(equal? (f \"-f\" \"arg0\")
        (f \"-f arg0\"))
;; > #t
"
  (apply exec-system* "fd" args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply f)
    (partial apply cdr)
    #;dbg)
   args))

