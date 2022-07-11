(define-module (gco)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gco) -s
!#
|#

(define (main args)
  ((compose
    (partial apply system*)
    (partial cons* "git" "checkout")
    cdr)
   args))
