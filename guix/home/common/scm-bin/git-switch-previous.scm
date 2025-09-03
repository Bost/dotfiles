(define-module (scm-bin git-switch-previous)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-command)
  #:export (main git-switch-previous))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-switch-previous) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-switch-previous.scm

# For some other git repository than the one in the $dots:

#!/usr/bin/env -S guile \\
-L /home/bost/dev/dotfiles/guix/common -L /home/bost/dev/dotfiles/guix/home/common -e (scm-bin\ git-switch-previous) -s
!#

$dotf/guix/home/common/scm-bin/git-switch-previous.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-switch-previous #:rest args)
  (git-command "switch" (append (list "-") args)))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply git-switch-previous)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
