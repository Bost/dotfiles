(define-module (restore-spacemacs)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 r5rs)
  #:use-module (utils)
  #:use-module (spag)                #| git-spacemacs |#
  #:export (main restore-spacemacs))

#|

#!/usr/bin/env -S guile \\
-L ./ -l spag.scm -e (restore-spacemacs) -s
!#

|#

(define* (restore-spacemacs #:rest args)

  #;
  (eval `(1+ ,(append `(+ 1 2) (list 3 4))) (interaction-environment))
  #;
  (primitive-eval `(1+ ,(append `(+ 1 2) (list 3 4))))
  ((compose
    (lambda (files)
      (let ((dir (str home "/.emacs.d/private")))
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
