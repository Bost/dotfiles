(define-module (ghog)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (ghog) -s
!#

|#

(define (main args)
  ((compose
    (partial
     map
     (compose
      cdr
      exec
      (lambda (remote)
        (append
         (list "git" "push" "--follow-tags" "--verbose" remote)
         (cdr args)))
      car))
    (partial filter (lambda (remote-url)
                      (not (null? (cdr remote-url)))))
    (partial map
             (lambda (remote)
               (cons remote
                     ((compose
                       (partial filter
                                (lambda (url)
                                  (string-match "git@" url)))
                       cdr
                       exec
                       (partial list "git" "remote" "get-url"))
                      remote))))
    (partial filter (lambda (remote)
                      (not (string-match "heroku" remote))))
    cdr
    exec)
   (list
    "git" "remote")))