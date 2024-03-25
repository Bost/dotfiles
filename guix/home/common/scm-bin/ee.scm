(define-module (scm-bin ee)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (guix monads)
  #:export (main ee))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ ee) -s
!#

cd $dotf
./guix/home/common/scm-bin/ee.scm
./guix/home/common/scm-bin/ee.scm --gx-dry-run

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (ee #:rest args)
  "
Usage:
(ee \"-f\" \"arg0\")
(ee \"-f arg0\")"
  (let* [(dry-run (contains--gx-dry-run args))
         (monad (if dry-run
                    compose-commands-guix-shell-dry-run
                    compose-commands-guix-shell))]
    (when dry-run
      (format #t "~a monad: ~a\n" m monad))
    (let* [(space-name "spacemacs")
           (dst (format #f "~a/.emacs.d.distros/~a-config/init.el"
                        (getenv "HOME") space-name))
           (src (format #f "~a/.~a" (getenv "dotf") space-name))]
      (with-monad monad
        (>>=
         (return (list dst))
         mdelete-file
         `(override-mv ,src ,dst)
         mcopy-file))
      (copy-file src dst))))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"arg0\")
(main \"<ignored>\" \"--gx-dry-run\" \"arg0\")
(main \"<ignored>\" \"--gx-dry-run\")
"
  ((comp
    (partial apply ee)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
