(define-module (scm-bin emacs-launcher)
  ;; for emacs-output-path
  #:use-module (gnu packages emacs)
  #:use-module (guix) ;; open-connection

  #:use-module (utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (emacs-launcher) -s
!#

|#

(format #t "~a ... " "Evaluating emacs-launcher.scm")

;; returns "/home/bost/.guix-home/profile/bin/emacs"
(define which-emacs
  (let* ((ret (exec "which emacs")))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          (car output)
          #| process output |#)
        (error-command-failed))))

;; returns "/gnu/store/c39qm5ql5w9r6lwwnhangxjby57hshws-emacs-28.2/bin/emacs"
(define emacs-output-path
  ((compose
    (partial format #f "~a/bin/emacs")
    derivation->output-path
    (partial package-derivation (open-connection)))
   emacs))

(define (main args)
  ((compose
    exec-background
    (partial cons*
             (compute-cmd
              "emacs"
              (str "emacsclient --no-wait --socket-name="
;;; See `dotspacemacs-server-socket-dir' in the `.spacemacs'.
;;; Tilda '~' doesn't work
                   "$HOME/.emacs.d" "/server/server")
              ;; which-emacs
              emacs-output-path))
    (lambda (prms) (if (null? prms) '("./") prms))
    cdr)
   args))

(format #t "done\n")
