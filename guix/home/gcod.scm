(define-module (gcod)
  #:use-module (utils)
  #:export (main gcod))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (gcod) -s
!#

|#

(define* (gcod #:rest args)
  "Usage:
(gcod  \"-f\" \"arg0\")
(gcod \"-f arg0\")
(equal? (gcod \"-f\" \"arg0\")
        (gcod \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "checkout" "-"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply gcod)
    (partial apply cdr)
    #;dbg)
   args))

