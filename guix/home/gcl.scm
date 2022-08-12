(define-module (gcl)
  #:use-module (utils)
  #:export (main gcl))

#|

#!/usr/bin/guile \
-l utils.scm -e (gcl) -s
!#

;; $HOME variable can't be used

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gcl) -s
!#

|#

(define* (gcl #:rest args)
  "Usage:
(gcl \"-f\" \"arg0\")
(gcl \"-f arg0\")
(equal? (gcl \"-f\" \"arg0\")
        (gcl \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "clone"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  (apply gcl (cdr args)))
