(define-module (scm-bin bat)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main bat)
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ bat) -s
!#

cd $dotf
./guix/home/common/scm-bin/bat.scm 

|#

(evaluating-module)

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
(testsymb 'main)

(module-evaluated)
