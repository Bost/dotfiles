(define-module (scm-bin restore-spacemacs)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (scm-bin spag)            #| git-spacemacs |#
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 r5rs)
  #:use-module (utils)
  #:use-module (settings)
  #:export (main restore-spacemacs))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ restore-spacemacs) -s
!#

cd $dotf
./guix/home/common/scm-bin/restore-spacemacs.scm 

|#

(evaluating-module)

(define* (restore-spacemacs #:rest args)
  #;
  (eval `(1+ ,(append `(+ 1 2) (list 3 4))) (interaction-environment))
  #;
  (primitive-eval `(1+ ,(append `(+ 1 2) (list 3 4))))
  ((compose
    (lambda (files)
      (let ((dir (str home "/" spacemacs-dir "/private")))
        (if (null? files)
            (format #t "Nothing to restore in ~a.\n" dir)
            ((compose
                exec
                (partial apply (partial git-spacemacs "restore"))
                #;
                (lambda (files)
                  (format #t "~a: ~a files need to be restored.\n" dir (length files))
                  files))
               files))))
    cdr
    exec)
   (git-spacemacs "ls-files --deleted")))

(define (main args)
  (restore-spacemacs))
(testsymb 'main)

(module-evaluated)
