(define-module (search-notes)
  #:use-module (utils)
  ;; #:use-module (guix build utils) #| invoke |#
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-L /home/bost/dev/dotfiles/guix/home -s
!#
|#

(define (main files args)
  (let* ((ret
          ((compose
            #;(lambda (p) (format #t "1: ~a\n" p) p)
            exec
            #;
            (lambda (p)
              ;; returns only <retval>
              (system* "search-notes" "-f" (string-append "'" files "'") "-p" "title")
              ;; returns only #t
              (invoke "search-notes" "-f" (string-append "'" files "'") "-p" "title"))
            #;(lambda (p) (format #t "0: ~a\n" p) p)
            (partial cons* (format #f "search-notes -f '~a' -p" files))
            cdr)
           args)))
    #;ret
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#
          (map (partial format #t "~a\n") output))
        (format #t "Command failed"))))

;; (main "shells" (list "_" "title"))
