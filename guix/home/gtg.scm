(define-module (gtg)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gtg) -s
!#
|#

(define (main args)
  ((compose
    (partial apply system*)
    (partial cons* "git" "tag" "--sort" "version:refname")
    cdr)
   args))
