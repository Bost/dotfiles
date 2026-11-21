(define-module (scm-bin timestamp)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (bost common utils)
  )

#|

#!/usr/bin/env -S guix repl --
!#

cd $dotf
# not '(apply main (command-line))'
echo -e "\n(main (command-line))" >> ./guix/home/common/scm-bin/timestamp.scm
./guix/home/common/scm-bin/timestamp.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define-public (main args)
  "Print the current local time, e.g. 2026-11-21_17-02-34, see `timestamp'.
ARGS, i.e. the whole command line, is ignored.
(main (list \"<ignored>\"))"
  (format #t "~a\n" (timestamp)))
(testsymb 'main)

(module-evaluated)
