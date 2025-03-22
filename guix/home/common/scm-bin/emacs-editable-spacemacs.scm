(define-module (scm-bin emacs-editable-spacemacs)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (settings)
  #:use-module (emacs-common)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ emacs-editable-spacemacs) -s
!#

cd $dotf
./guix/home/common/scm-bin/emacs-editable-spacemacs.scm --version
./guix/home/common/scm-bin/emacs-editable-spacemacs.scm --gx-dry-run
./guix/home/common/scm-bin/emacs-editable-spacemacs.scm --version --gx-dry-run
./guix/home/common/scm-bin/emacs-editable-spacemacs.scm

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
              #:fun set-config-editable
              #:profile spacemacs
              args))
(testsymb 'main)

(module-evaluated)
