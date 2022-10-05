(define-module (gtg)
  #:use-module (utils)
  #:export (main gtg))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (gtg) -s
!#

|#

(define* (gtg #:rest args)
  "Usage:
(gtg  \"-f\" \"arg0\")
(gtg \"-f arg0\")
(equal? (gtg \"-f\" \"arg0\")
        (gtg \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "tag" "--sort" "version:refname"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply gtg)
    (partial apply cdr)
    #;dbg)
   args))


