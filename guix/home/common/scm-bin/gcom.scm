(define-module (scm-bin gcom)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gcom))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (gcom) -s
!#

|#

(define* (gcom #:rest args)
  "Usage:
(gcom  \"-f\" \"arg0\")
(gcom \"-f arg0\")
(equal? (gcom \"-f\" \"arg0\")
        (gcom \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "checkout" "master" ;; TODO or main
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply gcom)
    (partial apply cdr)
    #;dbg)
   args))
