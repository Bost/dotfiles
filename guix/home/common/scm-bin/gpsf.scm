(define-module (scm-bin gpsf)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (scm-bin gps)
  #:export (main gpsf))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gpsf) -s
!#

cd $dotf
./guix/home/common/scm-bin/gpsf.scm

|#

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define* (gpsf #:rest args)
  "Usage:
"
  (let* ((ret (gps (cmd->string (append (list "--force") args)))))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          ;; process output
          ;; (map (partial format #t "output: ~a\n") output)
          ret)
        (begin
          (format #t "~a\n" (error-command-failed))
          *unspecified*))))
(testsymb 'gpsf)

(define* (main #:rest args)
  "Usage:
"
  ((compose
    (partial apply gpsf)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

;; (format #t "~a module evaluated\n" m)
