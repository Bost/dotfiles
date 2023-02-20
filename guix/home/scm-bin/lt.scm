(define-module (scm-bin lt)
  #:use-module (utils)
  #:export (main lt))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (lt) -s
!#

|#

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
         "exa" "-abghHliS" "--color=always" "--time-style=full-iso"
         "--sort=time" "--reverse"
         #|
         "exa" "-abghHliS" "--color=always"
         ;; exa has no support for '+%d-%m-%Y %H:%M:%S' time formatters
         "exa" "-abghHliS" "--color=always" "--time-style=default"
         "exa" "-abghHliS" "--color=always" "--time-style=iso"
         "exa" "-abghHliS" "--color=always" "--time-style=long-iso"
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
    (partial apply lt)
    (partial apply cdr)
    #;dbg)
   args))
