(define-module (gco)
  #:use-module (utils)
  #:export (main gco))

#|

#!/usr/bin/guile \
-l utils.scm -e (gco) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gco) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (gco) -s
!#

|#

(define* (gco #:rest args)
  "Usage: (gco \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply system*)
    dbg
    (partial append (list "git" "checkout"))
    flatten)
   args))

(define* (main #:rest args)
  "Usage: (main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose gco cdr) args))
