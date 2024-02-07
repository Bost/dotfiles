(define-module (scm-bin gtg)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gtg))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gtg) -s
!#

cd $dotf
./guix/home/common/scm-bin/gtg.scm 

|#

(evaluating-module)

(define* (gtg #:rest args)
  "Usage:
(gtg  \"-f\" \"arg0\")
(gtg \"-f arg0\")
(equal? (gtg \"-f\" \"arg0\")
        (gtg \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "tag" "--sort" "version:refname"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply gtg)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
