(define-module (scm-bin bat)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main bat)
  )

#|

#!/usr/bin/env -S guile \\
-L ./ -e (bat) -s
!#

|#

(define* (bat #:rest args)
  "Usage:
(bat  \"-f\" \"arg0\")
(bat \"-f arg0\")
(equal? (bat \"-f\" \"arg0\")
        (bat \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "bat" ;; "batcat" on ubuntu
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply bat)
    (partial apply cdr)
    #;dbg)
   args))

