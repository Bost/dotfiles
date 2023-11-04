(define-module (scm-bin grev)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main grev))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (grev) -s
!#

|#

(define* (grev #:rest args)
  "Usage:
(grev \"-f\" \"arg0\")
(grev \"-f arg0\")
(equal? (grev \"-f\" \"arg0\")
        (grev \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "remote"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply grev)
    (partial apply cdr)
    #;dbg)
   args))
