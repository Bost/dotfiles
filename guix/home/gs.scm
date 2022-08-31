(define-module (gs)
  #:use-module (utils)
  #:export (main gs))

#|
#!/usr/bin/guile \
-l utils.scm -e (gs) -s
!#

;; $HOME variable can't be used

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gs) -s
!#

|#

(define* (gs #:rest args)
  "Usage:
(gs \"-f\" \"arg0\")
(gs \"-f arg0\")
(equal? (gs \"-f\" \"arg0\")
        (gs \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "status" "--short" "--branch"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply gs)
    (partial apply cdr)
    #;dbg)
   args))
