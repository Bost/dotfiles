(define-module (scm-bin gps)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (scm-bin gre)
  #:export (main gps))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gps) -s
!#

cd $dotf
./guix/home/common/scm-bin/gps.scm

|#

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define* (gps #:rest args)
  "This git-pushes to all remote repos. TODO implement basic `gps`"
  ((compose
    (lambda (ret-vals) ;; the reducer
      ;; ret-vals is a list consisting of sublists:
      ;; ((ret-code-0 list-of-vals-0)
      ;;  (ret-code-N list-of-vals-N))
      ;; where ret-code-i is a number, list-of-vals-i can consist of any values
      (list (apply max (map car ret-vals)) (map cdr ret-vals)))
    
    (partial
     map
     (compose
      ;; if `git push ...` needs to return anything more except just a retcode -
      ;; implement it here
      exec
      (lambda (remote)
        (append
         (list "git" "push" "--follow-tags"
               ;; "--verbose"
               remote)
         args))
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
                       (partial gre "get-url"))
                      remote))))
    (partial filter (lambda (remote)
                      (not (string-match "heroku" remote))))
    cdr)
   (gre)))
(testsymb 'gps)

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"arg0\")"
  ((compose
    (partial apply gps)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

;; (format #t "~a module evaluated\n" m)
