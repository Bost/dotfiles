(define-module (scm-bin spag)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (settings)
  #:export (main git-spacemacs))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (spag) -s
!#

|#

(define* (git-spacemacs #:rest args)
  (let ((sp-path (str home "/" spacemacs-dir)))
    (cons* "git"
           (str "--git-dir=" sp-path "/.git")
           (str "--work-tree=" sp-path)
           args)))

(define (main args)
  (map exec
       (list
        (git-spacemacs "fetch" "--tags" "origin" "develop")
        (git-spacemacs "rebase" "origin/develop" "develop")
        (git-spacemacs "rebase" "develop" "cycle"))))
