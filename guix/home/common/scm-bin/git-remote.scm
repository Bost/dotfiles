(define-module (scm-bin git-remote)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-command)
  #:export (main git-remote))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-remote) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-remote.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-remote #:rest args)
  (apply (partial git-command "remote") args))

(define* (main #:rest args)
  "Usage: "
  ((comp
    (partial apply git-remote)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
