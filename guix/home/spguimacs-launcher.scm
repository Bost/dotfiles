(define-module (spguimacs-launcher)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (spguimacs-launcher) -s
!#

|#

(format #t "~a ... " "Evaluating spguimacs-launcher.scm")
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
(format #t "done\n")
