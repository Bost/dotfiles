(define-module (gcl)
  #:use-module (utils)
  #:export (main gcl)
  )

#|

#!/usr/bin/guile \
-l utils.scm -e (gcl) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gcl) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (gcl) -s
!#

|#

(define* (gcl #:rest args)
  "Usage: (gcl \"-f\" \"arg0\")"
  ((compose
    (partial apply system*)
    dbg
    (partial append (list "git" "clone"))
    flatten)
   args))

(define* (main #:rest args)
  "Usage: (main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose gcl cdr) args))
