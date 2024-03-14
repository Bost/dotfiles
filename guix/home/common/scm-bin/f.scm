(define-module (scm-bin f)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main f))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ f) -s
!#

cd $dotf
./guix/home/common/scm-bin/f.scm 

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (f #:rest args)
  "Usage:
(f  \"-f\" \"arg0\")
(f \"-f arg0\")
(equal? (f \"-f\" \"arg0\")
        (f \"-f arg0\"))
;; > #t
"
  (apply exec-system* "fd" args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply f)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
