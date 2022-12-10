(define-module (emacs-launcher)
  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (emacs-launcher) -s
!#

|#

(define init-cmd "emacs")
(define client-cmd (str "emacsclient --no-wait --socket-name="
;;; See `dotspacemacs-server-socket-dir' in the .spacemacs
;;; Tilda '~' doesn't work
                 "$HOME/.emacs.d"
                 "/server/server"))
(define pattern
  (let* ((ret (exec "which emacs")))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          (car output)
          #| process output |#)
        (error-command-failed))))

(define (main args)
  ((compose
    exec-background
    ;; dbg
    (partial cons* (compute-cmd init-cmd client-cmd pattern))
    (lambda (prms) (if (null? prms) '("./") prms))
    cdr)
   args))
