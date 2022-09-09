(define-module (gre)
  #:use-module (utils)
  #:export (main gre))

#|

#!/run/current-system/profile/bin/guile \
-l utils.scm -e (gre) -s
!#

|#

(define* (gre #:rest args)
  "Usage:
(gre \"-f\" \"arg0\")
(gre \"-f arg0\")
(equal? (gre \"-f\" \"arg0\")
        (gre \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "remote"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply gre)
    (partial apply cdr)
    #;dbg)
   args))
