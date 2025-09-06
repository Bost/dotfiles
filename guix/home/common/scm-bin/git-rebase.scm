(define-module (scm-bin git-rebase)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-command)
  #:export (main git-rebase))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-rebase) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-rebase.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-rebase #:rest args)
  (apply (partial git-command "rebase") args))

(define* (main #:rest args)
  "Usage: "
  ((comp
    (partial apply git-rebase)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
