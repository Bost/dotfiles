(define-module (scm-bin grev)
;;; All used modules must be present in the module (services scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-remote)
  #:export (main grev))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ grev) -s
!#

cd $dotf
./guix/home/common/scm-bin/grev.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (grev #:rest args)
  "Usage:
"
  (let* ((ret (git-remote (cmd->string (append (list "--verbose") args)))))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          ;; process output
          (map (partial format #t "~a\n") output)
          ret)
        (begin
          (error-command-failed m)
          *unspecified*))))
(testsymb 'grev)

(define* (main #:rest args)
  "Usage:
"
  ((comp
    (partial apply grev)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
