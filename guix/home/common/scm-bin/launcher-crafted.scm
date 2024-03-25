(define-module (scm-bin launcher-crafted)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #|
  #:use-module (gnu packages emacs) ; for emacs-output-path
  #:use-module (guix)               ; for open-connection
  ;; Activating the two lines above leads to:
  ;;   In procedure resolve-interface: no code for module (gnu packages emacs)
  ;;   In procedure resolve-interface: no code for module (guix)
  |#
  #:use-module (utils) ;; partial
  #:use-module (scm-bin launcher-emacs)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ launcher-crafted) -s
!#

cd $dotf
./guix/home/common/scm-bin/launcher-crafted.scm rest args
./guix/home/common/scm-bin/launcher-crafted.scm --profile=my-profile rest args

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (main args)
  (apply (partial launcher-emacs #:profile "crafted")
         (cdr args)))

(module-evaluated)
