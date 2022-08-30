(define-module (bat)
  #:use-module (utils)
  #:export (main bat)
  )

#|

#!/usr/bin/guile \
-l utils.scm -e (bat) -s
!#

;; $HOME variable can't be used

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (bat) -s
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

