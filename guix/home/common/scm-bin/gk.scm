(define-module (scm-bin gk)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (bost common utils))

#|

#!/usr/bin/env -S guix repl --
!#

cd $dotf
echo -e "\n(apply main (command-line))" >> ./guix/home/common/scm-bin/gk.scm
./guix/home/common/scm-bin/gk.scm

|#

;; (define m (module-name-for-logging))
;; (evaluating-module)

(define-public (main . args)
  ((comp
    exec-background
    (lambda (p) (append '("gitk") (if (null? p) '("--all") p)))
    cdr)
   args))
;; (testsymb 'main)
;; (module-evaluated)
