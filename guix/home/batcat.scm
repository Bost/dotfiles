(define-module (batcat)
  #:use-module (utils)
  #:export (main batcat))

#|

#!/usr/bin/guile \
-l utils.scm -e (batcat) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (batcat) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (batcat) -s
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
         "bat"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  (apply bat (cdr args)))

