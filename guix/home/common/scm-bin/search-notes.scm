(define-module (scm-bin search-notes)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  ;; #:use-module (guix build utils) #| invoke |#
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ search-notes) -s
!#

cd $dotf
./guix/home/common/scm-bin/search-notes.scm 

|#

(evaluating-module)

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
;;; TODO compile search-notes if it doesn't exits in the PATH.
;;; See $der/search-notes/README.md
            (partial cons* (format #f "search-notes -e '~a' -p" files))
            cdr)
           args)))
    #;ret
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#
          (map (partial format #t "~a\n") output))
        (error-command-failed))))
(testsymb 'main)

(module-evaluated)

;; (main "shells" (list "_" "title"))
