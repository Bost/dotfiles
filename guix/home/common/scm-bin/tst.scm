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
(main \"<ignored>\" \"arg0\")"

  (with-monad compose-shell-commands
    (return "/tmp/fox"))

  (with-monad compose-shell-commands
    (>>=
     (return "uname -o")
     exec   ; => (0 "GNU/Linux")
     (partial echo #:string)))

  (with-monad compose-guix-shell-commands
    (return '("/tmp/fox")))

  (with-monad compose-guix-shell-commands
    (>>=
     (return '("/tmp/fox"))
     mdelete-file
     `(override-mv ,(string-append (getenv "HOME") "/.bashrc") "/tmp/fox")
     mcopy-file
     )))

(testsymb 'main)

(module-evaluated)
