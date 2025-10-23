(define-module (scm-bin gpsf)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (scm-bin gps)
  #:export (main gpsf gpsf-all))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gpsf) -s
!#

cd $dotf
./guix/home/common/scm-bin/gpsf.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

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
          (error-command-failed m)
          *unspecified*))))
(testsymb 'gpsf)

(define* (gpsf-all #:rest args)
  "Usage:
"
  (let* ((ret (gps-all (cmd->string (append (list "--force") args)))))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          ;; process output
          ;; (map (partial format #t "output: ~a\n") output)
          ret)
        (begin
          (error-command-failed m)
          *unspecified*))))
(testsymb 'gpsf-all)

(define* (main #:rest args)
  "Usage:
"
  ((comp
    (partial apply gpsf-all)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
