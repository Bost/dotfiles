(define-module (f)
  #:use-module (utils)
  #:export (main f))

#|

#!/usr/bin/guile \
-l utils.scm -e (f) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (f) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (f) -s
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
  (apply f (cdr args)))

