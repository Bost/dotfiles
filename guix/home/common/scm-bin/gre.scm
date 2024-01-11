(define-module (scm-bin gre)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gre))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gre) -s
!#

cd $dotf
./guix/home/common/scm-bin/gre.scm 

|#

(define m (module-name-for-logging))
(evaluating-module m)

(define* (gre #:rest args)
  "Usage: "
  (let* ((ret (exec (append 
                     (list "git" "remote")
                     args))))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          ;; process output
          (map (partial format #t "~a\n") output)
          ret)
        (begin
          (format #t "~a\n" (error-command-failed))
          *unspecified*))))
(testsymb 'gre)

(define* (main #:rest args)
  "Usage: "
  ((compose
    (partial apply gre)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated m)
