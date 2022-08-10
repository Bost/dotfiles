(define-module (chmod)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (chmod) -s
!#
|#

(define (main modifier args)
  ((compose
    exec
    (partial cons* (string-append "chmod +" modifier))
    cdr)
   args))
