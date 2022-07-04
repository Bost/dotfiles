(define-module (batcat)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (batcat) -s
!#
|#

(define (main args)
  (let* ((ret
          ((compose
            ;; TODO implement exec-no-read-line
            #;exec ;; causes color loss
            (partial apply system*)
            (partial cons* "bat")
            cdr)
           args)))
    ret
    #;(if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#
          (map (partial format #t "~a\n") output))
        (format #t "Command failed"))))
