(define-module (scm-bin emacs-launcher-crafted)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (settings)
  #:use-module (emacs-common)
  #:export (main))

#|
;; -e calls the `main` function

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ emacs-launcher-crafted) -s
!#

cd $dotf
./guix/home/common/scm-bin/emacs-launcher-crafted.scm --version

set f $dotf/guix/home/common/scm-bin/emacs-launcher-crafted.scm
./guix/home/common/scm-bin/emacs-launcher-crafted.scm $f

|#

(define m (module-name-for-logging))
(evaluating-module)

(define utility-name (last (module-name (current-module))))

(define (main args)
  "Usage:
(main (list \"<ignored>\"))
(main (list \"<ignored>\" \"--help\" \"args\"))
(main (list \"<ignored>\" \"rest\" \"args\"))
(main (list \"<ignored>\" \"--gx-dry-run\" \"rest\" \"args\"))
"
  (handle-cli #:utility-name utility-name
              #:fun create-emacs-launcher
              #:profile crafted
              args))
(testsymb 'main)

(module-evaluated)
