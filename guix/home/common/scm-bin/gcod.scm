(define-module (scm-bin gcod)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gcod))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gcod) -s
!#

cd $dotf
./guix/home/common/scm-bin/gcod.scm 

|#

(evaluating-module)

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
  ((comp
    (partial apply gcod)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
