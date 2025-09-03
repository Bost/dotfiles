(define-module (scm-bin gicm)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-checkout)
  #:export (main gicm))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gicm) -s
!#

cd $dotf
./guix/home/common/scm-bin/gicm.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-checkout #:rest args)
  (apply (partial git-checkout "master") args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply gicm)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
