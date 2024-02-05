(define-module (scm-bin gpg-pinentry-setup)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main gpg-pinentry-setup))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gpg-pinentry-setup) -s
!#

cd $dotf
./guix/home/common/scm-bin/gpg-pinentry-setup.scm

|#

(evaluating-module)

(define* (gpg-pinentry-setup #:rest args)
  (let [(pinentry-binary "pinentry-gtk-2" #;"pinentry-tty")]
    (when ((compose
            exec-system*
            (partial format #f "pgrep --full gpg-agent.*~a"))
           pinentry-binary)
      (exec-system* "pkill gpg-agent"))
    ((compose
      exec-system*
      (partial format #f "gpg-agent --pinentry-program=~a --daemon")
      cadr
      exec
      (partial format #f "which ~a"))
     pinentry-binary)))

(define* (main #:rest args)
  "Usage:
(main (list \"<ignored>\"))"
  ((compose
    (partial apply gpg-pinentry-setup)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
