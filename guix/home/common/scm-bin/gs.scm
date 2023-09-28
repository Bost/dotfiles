(define-module (scm-bin gs)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gs))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (gs) -s
!#

|#

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
  ((compose
    (partial apply gs)
    (partial apply cdr)
    #;dbg)
   args))
