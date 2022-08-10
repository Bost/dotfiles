(define-module (ls)
  #:use-module (utils)
  #:export (main ls))

#|

#!/usr/bin/guile \
-l utils.scm -e (ls) -s
!#

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (ls) -s
!#

#!$HOME/.guix-home/profile/bin/guile \
-l utils.scm -e (ls) -s
!#

|#

(define* (ls #:rest args)
  "Usage:
(ls \"-f\" \"arg0\")
(ls \"-f arg0\")
(equal? (ls \"-f\" \"arg0\")
        (ls \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "exa" "-abghHliS" "--color=always" "--time-style=full-iso"
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
  (apply ls (cdr args)))
