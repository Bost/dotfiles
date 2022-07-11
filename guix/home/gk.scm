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
    (lambda (p) (format #t "0: ~a\n" p) p)
    (lambda (p) (append '("gitk") (if (null? p) '("--all") p) '("&")))
    cdr)
   args))
