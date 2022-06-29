(define-module (batcat)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-L /home/bost/dev/dotfiles/guix/home -s
!#
|#

(define (main args)
  (let* ((ret
          ((compose
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

;; (main (command-line))
