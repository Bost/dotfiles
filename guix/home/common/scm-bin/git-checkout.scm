(define-module (scm-bin git-checkout)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-command)
  #:export (main git-checkout))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-checkout) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-remote.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-checkout #:rest args)
  (apply (partial git-command "checkout") args))

(define* (main #:rest args)
  "Usage: "
  ((comp
    (partial apply git-checkout)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
