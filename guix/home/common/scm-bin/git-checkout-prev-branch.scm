(define-module (scm-bin git-checkout-prev-branch)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main git-checkout-prev-branch))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-checkout-prev-branch) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-checkout-prev-branch.scm 

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-checkout-prev-branch #:rest args)
  "Usage:
(git-checkout-prev-branch  \"-f\" \"arg0\")
(git-checkout-prev-branch \"-f arg0\")
(equal? (git-checkout-prev-branch \"-f\" \"arg0\")
        (git-checkout-prev-branch \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "checkout" "-"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply git-checkout-prev-branch)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
