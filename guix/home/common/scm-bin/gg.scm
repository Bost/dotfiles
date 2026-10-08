(define-module (scm-bin gg)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (bost common utils)
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|

#!/usr/bin/env -S guix repl --
!#

cd $dotf
# not '(apply main (command-line))'
echo -e "\n(main (command-line))" >> ./guix/home/common/scm-bin/gg.scm
./guix/home/common/scm-bin/gg.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define-public (main args)
  ((comp
    exec-background
    (partial append (list "git" "gui"))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)
