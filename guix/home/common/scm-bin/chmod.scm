(define-module (scm-bin chmod)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main chmod))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (chmod) -s
!#

|#

;; new implementation
(define* (chmod #:rest args)
  (apply exec-system*
         "chmod" (string-append "+" (car args))
         (cdr args)))

;; (define* (main #:rest args)
;;   "Usage:
;; (main \"<ignored>\" \"-f\" \"arg0\")"
;;   ((compose
;;     (partial apply chmod)
;;     (partial apply cdr)
;;     dbg)
;;    args))


;; old implementation
(define (main modifier args)
  ((compose
    exec
    (partial cons* (string-append "chmod +" modifier))
    cdr)
   args))

