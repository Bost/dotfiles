(define-module (gg)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gg) -s
!#
|#

(define (main args)
  ((compose
    exec-background
    (lambda (p) (format #t "0: ~a\n" p) p)
    (lambda (p) (append '("git" "gui") p '("&")))
    cdr)
   args))
