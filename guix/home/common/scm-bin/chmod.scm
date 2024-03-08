(define-module (scm-bin chmod)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main chmod))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ chmod) -s
!#

cd $dotf
./guix/home/common/scm-bin/chmod.scm 

|#

(evaluating-module)

;; new implementation
(define* (chmod #:rest args)
  (apply exec-system*
         "chmod" (string-append "+" (car args))
         (cdr args)))

;; (define* (main #:rest args)
;;   "Usage:
;; (main \"<ignored>\" \"-f\" \"arg0\")"
;;   ((comp
;;     (partial apply chmod)
;;     (partial apply cdr)
;;     dbg)
;;    args))


;; old implementation
(define (main modifier args)
  ((comp
    exec
    (partial cons* (string-append "chmod +" modifier))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)
