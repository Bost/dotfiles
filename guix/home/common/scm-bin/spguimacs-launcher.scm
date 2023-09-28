(define-module (scm-bin spguimacs-launcher)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  ;; #:use-module (bost utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (spguimacs-launcher) -s
!#

|#
(define m (module-name-for-logging))
;; (format #t "~a evaluating ...\n" m)

(define init-cmd "spacemacs")

(define client-cmd (str "emacsclient --no-wait --socket-name="
;;; See `dotspacemacs-server-socket-dir' in the .spguimacs
;;; Tilda '~' doesn't work
                 "$HOME/.local/share/spacemacs"
                 "/server/server"))

(define pattern
  "spacemacs-start-directory"
  #;
  (let* ((ret (exec "which spacemacs")))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          (car output)
          #| process output |#)
        (error-command-failed))))

(define (main args)
  ((compose
    exec-background
    (partial cons* (compute-cmd init-cmd client-cmd pattern))
    (lambda (prms) (if (null? prms) '("./") prms))
    cdr)
   args))

(format #t "~a module evaluated\n" m)
