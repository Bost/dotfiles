(define-module (emacs-launcher)
  #:use-module (utils)
  #:use-module (srfi srfi-1) #| find |#
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (emacs-launcher) -s
!#

|#

;; TODO use continuation breakout instead of a global variable
(define binary-init "emacs")
(define binary binary-init)

(define (compute-binary)
  (let ((user (getenv "USER")))
    ((compose
      (lambda (p)
        (if (null? p)
            binary-init ;; No such binary has been started yet.
            (car p)))
      (partial
       map
       (lambda (pid)
         (when (string=? binary binary-init)
           (let ((proc-user ((compose
                              cadr
                              exec)
                             ;; -o means: user specified format
                             (format #f "ps -o user= -p ~a" pid))))
             (if (and (not (string-null? proc-user))
                      (string=? user proc-user))
                 (let ((proc-cmd (exec
                                  (format #f "ps -o command= -p ~a" pid))))
                   (set! binary
                         (if #f
                             #| string match --quiet -- "*defunct*" proc-cmd |#
                             binary-init
                             (str "emacsclient --no-wait --socket-name="
;;; See `dotspacemacs-server-socket-dir' in the .spacemacs
;;; Tilda '~' doesn't work
                                  "$HOME/.emacs.d"
                                  "/server/server")))))))
         binary))
      cdr
      exec
      (partial format #f "pgrep --full -u ~a ~a" user))
     (let* ((ret (exec "which emacs")))
       (if (= 0 (car ret))
           (let* ((output (cdr ret)))
             (car output)
             #| process output |#)
           (error-command-failed))))))

(define (main args)
  ((compose
    exec-background
    (partial cons* (compute-binary))
    (lambda (prms) (if (null? prms) '("./") prms))
    cdr)
   args))
