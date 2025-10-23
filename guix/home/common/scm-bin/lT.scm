(define-module (scm-bin lT)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main lT))

#|

;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ lT) -s
!#

cd $dotf
./guix/home/common/scm-bin/lT.scm /home/bost/

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (lT #:rest args)
  "List directory content sorted by time, oldest on top.

Usage:
(lT \"-f\" \"arg0\")
(lT \"-f arg0\")
(equal? (lT \"-f\" \"arg0\")
        (lT \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "eza" "-abghHliS"
         "--color=always" "--time-style=+%d-%m-%Y\\ %H:%M:%S"
         "--sort=time"
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
    (partial apply lT)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
