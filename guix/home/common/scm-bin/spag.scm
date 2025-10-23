(define-module (scm-bin spag)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (settings)
  #:export (main git-spacemacs))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ spag) -s
!#

cd $dotf
./guix/home/common/scm-bin/spag.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-spacemacs #:rest args)
  (let ((sp-path (str home "/" spacemacs-dir)))
    (cons* "git"
           (str "--git-dir=" sp-path "/.git")
           (str "--work-tree=" sp-path)
           args)))

(define (main args)
  (map exec
       (list
        (git-spacemacs "fetch" "--tags" origin develop)
        (git-spacemacs "rebase" (str origin "/" develop) develop)
        (git-spacemacs "rebase" develop cycle)
        (git-spacemacs "rebase" cycle guix)
        )))
(testsymb 'main)

(module-evaluated)
