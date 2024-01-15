(define-module (scm-bin spguimacs-launcher)
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
  #:use-module (settings)
  ;; #:use-module (bost utils)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (spguimacs-launcher) -s
!#

|#

;; (format #t "~a evaluating ...\n" m)

(define (emacs-output-path)
  "(emacs-output-path)
=> \"/gnu/store/c39qm5ql5w9r6lwwnhangxjby57hshws-emacs-28.2/bin/emacs\""
  ((compose
    (partial format #f "~a/bin/emacs")
    derivation->output-path
    (partial package-derivation (open-connection)))
   emacs))

(define (which-emacs)
  "(which-emacs) => \"/home/bost/.guix-home/profile/bin/emacs\""
  ;; (emacs-output-path)
  (which "emacs"))

(define (which-emacsclient)
  "(which-emacsclient) => \"/home/bost/.guix-home/profile/bin/emacsclient\""
  (which "emacsclient"))

(define (main args)
  (let* [(profile "spguimacs")
         (emacs-bin (which-emacs))
         (init-cmd (cmd->string
                    (list emacs-bin (str "--with-profile=" profile) "--daemon")))]
    ((compose
      (lambda (cmd) ((if (string= cmd init-cmd) exec exec-background) cmd))
;;; Search for the full command line:
;;; $ pgrep --full --euid bost "/home/bost/.guix-home/profile/bin/emacs --with-profile=spguimacs --daemon"
      (lambda (client-cmd) (compute-cmd init-cmd client-cmd init-cmd))
      cmd->string
      (partial append (list (which-emacsclient) "--create-frame"
                            (str "--socket-name=" profile)))
      (lambda (prms) (if (null? prms) '("./") prms))
      cdr)
     args)))

(format #t "~a module evaluated\n" m)
