(define-module (gcom)
  #:use-module (utils)
  #:export (main gcom))

#|

#!/usr/bin/guile \
-l utils.scm -e (gcom) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gcom) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (gcom) -s
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
  (apply gcom (cdr args)))

