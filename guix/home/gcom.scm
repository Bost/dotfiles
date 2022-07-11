(define-module (gcom)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gcom) -s
!#
|#

(define (main args)
  ((compose
    (partial apply system*)
    (partial cons* "git" "checkout"
             "master" ;; TODO or main
             )
    cdr)
   args))
