(define-module (f)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (f) -s
!#
|#

(define (main args)
  ((compose
    (partial apply system*)
    #;(lambda (p) (format #t "0: ~a\n" p) p)
    (partial cons* "fd")
    cdr)
   args))
