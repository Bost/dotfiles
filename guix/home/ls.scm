(define-module (ls)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (ls) -s
!#
|#

(define (main args)
  ((compose
    ;; TODO `exec' doesn't work with exa. WTF?
    (partial apply system*)
    #;(lambda (p) (format #t "#t before system*/exec: ~a\n" p) p)
    (partial
     cons*
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
     |#)
    cdr)
   args))
