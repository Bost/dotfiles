(define-module (scm-bin lt)
;;; All used modules must be present in the module (services scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main lt))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ lt) -s
!#

cd $dotf
./guix/home/common/scm-bin/lt.scm /home/bost/

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (lt #:rest args)
  "List directory content sorted by time, youngest on top.

Usage:
(lt \"-f\" \"arg0\")
(lt \"-f arg0\")
(equal? (lt \"-f\" \"arg0\")
        (lt \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "eza" "-abghHliS"
         "--color=always" "--time-style=+%d-%m-%Y\\ %H:%M:%S"
         "--sort=time" "--reverse"
         #|
         "eza" "-abghHliS" "--color=always"
         "eza" "-abghHliS" "--color=always" "--time-style=default"
         "eza" "-abghHliS" "--color=always" "--time-style=iso"
         "eza" "-abghHliS" "--color=always" "--time-style=long-iso"
         ;; '--file-type' append indicator (one of /=>@|) to entries
         ;; custom coloring can be done after after `ls --color=never`
         "ls" "-lA" "--file-type" "--color"
         "--time-style=+%d-%m-%Y\\ %H:%M:%S"
         |#
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply lt)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
