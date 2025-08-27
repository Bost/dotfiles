(define-module (scm-bin git-remote)
;;; All used modules must be present in the module (services scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
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
          (error-command-failed m)
          *unspecified*))))
(testsymb 'git-remote)

(define* (main #:rest args)
  "Usage: "
  ((comp
    (partial apply git-remote)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
