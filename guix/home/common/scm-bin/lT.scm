(define-module (scm-bin lT)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main lT))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ lT) -s
!#

cd $dotf
./guix/home/common/scm-bin/lT.scm /home/bost/

|#

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
         "eza" "-abghHliS" "--color=always" "--time-style=full-iso"
         "--sort=time"
         #|
         "eza" "-abghHliS" "--color=always"
         ;; exa has no support for '+%d-%m-%Y %H:%M:%S' time formatters
         "eza" "-abghHliS" "--color=always" "--time-style=default"
         "eza" "-abghHliS" "--color=always" "--time-style=iso"
         "eza" "-abghHliS" "--color=always" "--time-style=long-iso"
         ;; '--file-type' append indicator (one of /=>@|) to entries
         ;; TODO consider custom coloring after `ls --color=never`
         "ls" "-lA" "--file-type" "--color"
         "--time-style=+%d-%m-%Y %H:%M:%S"
         |#
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((compose
    (partial apply lT)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
