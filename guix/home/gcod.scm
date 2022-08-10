(define-module (gcod)
  #:use-module (utils)
  #:export (main gcod))

#|

#!/usr/bin/guile \
-l utils.scm -e (gcod) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gcod) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (gcod) -s
!#

|#

(define* (gcod #:rest args)
  "Usage:
(gcod  \"-f\" \"arg0\")
(gcod \"-f arg0\")
(equal? (gcod \"-f\" \"arg0\")
        (gcod \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "checkout" "-"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  (apply gcod (cdr args)))

