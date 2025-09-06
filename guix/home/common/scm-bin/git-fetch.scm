(define-module (scm-bin git-fetch)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-command)
  #:export (main git-fetch))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-fetch) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-fetch.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-fetch #:rest args)
  (apply (partial git-command "fetch") args))

(define* (main #:rest args)
  "Usage: "
  ((comp
    (partial apply git-fetch)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
