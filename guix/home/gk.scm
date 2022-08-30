(define-module (gk)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gk) -s
!#
|#

(define (main args)
  ((compose
    exec-background
    dbg
    (lambda (p) (append '("gitk") (if (null? p) '("--all") p) '("&")))
    cdr)
   args))
