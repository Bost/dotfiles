(define-module (scm-bin tst)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (guix monads)
  #:use-module (scm-bin echo)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ tst) -s
!#

cd $dotf
./guix/home/common/scm-bin/tst.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"arg0\")
(main \"<ignored>\" \"--gx-dry-run\" \"arg0\")
(main \"<ignored>\" \"--gx-dry-run\")
"

  (with-monad compose-shell-commands
    (return "/tmp/fox"))

  (with-monad compose-shell-commands
    (>>=
     (return "uname -o")
     exec   ; => (0 "GNU/Linux")
     (partial echo #:string)))

  (with-monad compose-commands-guix-shell
    (return '("/tmp/fox")))

  (with-monad compose-commands-guix-shell
    (>>=
     (return '("/tmp/fox"))
     mdelete-file
     `(override-mv ,(string-append (getenv "HOME") "/.bashrc") "/tmp/fox")
     mcopy-file
     ))

  (let* [(dry-run (contains--gx-dry-run args))
         (monad (if dry-run
                    compose-commands-guix-shell-dry-run
                    compose-commands-guix-shell))]
    (when dry-run
      (format #t "~a monad: ~a\n" m monad))
    (with-monad monad
      (>>=
       (return '("/tmp/fox"))
       mdelete-file
       `(override-mv ,(string-append (getenv "HOME") "/.bashrc") "/tmp/fox")
       mcopy-file
       ))))

(testsymb 'main)

(module-evaluated)
