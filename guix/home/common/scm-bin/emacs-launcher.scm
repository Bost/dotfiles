(define-module (scm-bin emacs-launcher)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #|
  #:use-module (gnu packages emacs) ; for emacs-output-path
  #:use-module (guix)               ; for open-connection
  ;; Activating the two lines above leads to:
  ;;   In procedure resolve-interface: no code for module (gnu packages emacs)
  ;;   In procedure resolve-interface: no code for module (guix)
  |#

  #:use-module (utils)
  ;; #:use-module (bost utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (emacs-launcher) -s
!#

|#

(define m (module-name-for-logging))
;; (format #t "~a evaluating ...\n" m)

;; returns "/home/bost/.guix-home/profile/bin/emacs"
(define (which-emacs)
  (let* ((ret (exec "which emacs")))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          (car output)
          #| process output |#)
        (error-command-failed))))

;; returns "/gnu/store/c39qm5ql5w9r6lwwnhangxjby57hshws-emacs-28.2/bin/emacs"
;; (define (emacs-output-path)
;;   ((compose
;;     (partial format #f "~a/bin/emacs")
;;     derivation->output-path
;;     (partial package-derivation (open-connection)))
;;    emacs))

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
              ;; (emacs-output-path)
              (which-emacs)))
    (lambda (prms) (if (null? prms) '("./") prms))
    cdr)
   args))

(format #t "~a module evaluated\n" m)
