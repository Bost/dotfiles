(define-module (scm-bin susp)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main susp))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ susp) -s
!#

cd $dotf
./guix/home/common/scm-bin/susp.scm

|#

(evaluating-module)

(define* (susp #:rest args)
  "Usage: "
  (apply exec-system* (append
                       (list "xfce4-session-logout" "--suspend")
                       args)))
(testsymb 'susp)

(define* (main #:rest args)
  "Usage: "
  ((comp
    (partial apply susp)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
