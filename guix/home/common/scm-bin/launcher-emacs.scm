(define-module (scm-bin launcher-emacs)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils) ;; partial
  #:use-module (settings)

  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (main launcher-emacs))

#|
;; -e calls the `main` function

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ launcher-emacs) -s
!#

cd $dotf
./guix/home/common/scm-bin/launcher-emacs.scm rest args
./guix/home/common/scm-bin/launcher-emacs.scm --profile=my-profile rest args


./guix/home/common/scm-bin/launcher-emacs.scm --profile=spacemacs ~/.emacs.d.distros/spguimacs-config/.spacemacs
./guix/home/common/scm-bin/launcher-emacs.scm --profile=spguimacs ~/.emacs.d.distros/spguimacs-config/.spacemacs

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (emacs-binary-path)
  "(emacs-binary-path)
=> \"/gnu/store/09a50cl6ndln4nmp56nsdvn61jgz2m07-emacs-29.1/bin/emacs\""
  ((comp
    (partial format #f "~a/bin/emacs")
    package-output-path)
   (@(gnu packages emacs) emacs)))

(define (which-emacs)
  "(which-emacs) => \"/home/bost/.guix-home/profile/bin/emacs\""
  ;; (emacs-binary-path)
  ((@(guix build utils) which) "emacs"))

(define (which-emacsclient)
  "(which-emacsclient) => \"/home/bost/.guix-home/profile/bin/emacsclient\""
  ((@(guix build utils) which) "emacsclient"))

(define* (launcher-emacs #:key profile #:rest args)
  "
(launcher-emacs #:profile \"my-profile\" \"rest\" \"args\")
"
  (let* ((args (remove-kw-from-args #:profile args)))
    ;; (format #t "~a profile : ~a\n" m profile)
    ;; (format #t "~a args : ~a\n" m args)
    (let* [(emacs-bin (which-emacs))
           (init-cmd
            (cmd->string
             (append
              (list emacs-bin
                    (str "--with-profile=" profile)
                    ;; (str "--init-directory=$HOME/.emacs.d.distros/" profile)
                    "--daemon")
              ;; the init-cmd must not contain the args otherwise the `pgrep
              ;; ...` detection won't work
              #;args)))]
      ((comp
        ;; (lambda (p) (format #t "4:\n~a\n" p) p)
;;; Search for the full command line:
;;; $ pgrep --full --euid bost "/home/bost/.guix-home/profile/bin/emacs --with-profile=spacemacs --daemon"
        (lambda (client-cmd)
          (if (string=? (compute-cmd init-cmd client-cmd init-cmd)
                        client-cmd)
              (exec-background client-cmd)
              (when (zero? (car (exec init-cmd)))
                ;; Calling (exec-background cmd) makes sense only if the emacs
                ;; server has been started successfully.
                (exec-background client-cmd))))
        ;; (lambda (p) (format #t "3:\n~a\n" p) p)
        cmd->string
        ;; (lambda (p) (format #t "2:\n~a\n" p) p)
        (partial append (list (which-emacsclient) "--create-frame"
                              (str "--socket-name=" profile)))
        ;; (lambda (p) (format #t "1:\n~a\n" p) p)
        (lambda (prms) (if (null? prms) '("./") prms))
        ;; (lambda (p) (format #t "0:\n~a\n" p) p)
        ;; cdr
        )
       args))))

(define (main args)
  "
(main (list \"launcher-emacs\" \"rest\" \"args\"))
(main (list \"launcher-emacs\" \"--profile=aaa\" \"rest\" \"args\"))
"
  ;; (format #t "[main] args: ~a\n" args)
  (let* ((option-spec
          ;; (value #t): a given option expects accept a value
          ;; 'profile' corresponds to '--profile' or '-p' on the command line
          '(
            (profile (single-char #\p) (value #t))
            (version (single-char #\v) (value #f))
            (help    (single-char #\h) (value #f))
            (rest-args (value #f))
            )))
    ;; (format #t "[main] option-spec : ~a\n" option-spec)
    (let*
        ((options (getopt-long args option-spec))
         ;; #f means that the expected value wasn't specified
         (profile (option-ref options 'profile "spacemacs"))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (rest-args (option-ref options '() #f))
         )
      ;; (format #t "[main] options : ~a\n" options)
      ;; (format #t "[main] profile : ~a\n" profile)
      ;; (format #t "[main] (string? profile) : ~a\n" (string? profile))
      ;; (format #t "[main] help-wanted : ~a\n" help-wanted)
      ;; (format #t "[main] version-wanted : ~a\n" version-wanted)
      (format #t "[main] rest-args : ~a\n" rest-args)
      (if (or version-wanted help-wanted)
          (begin
            (if help-wanted
                (format #t "
      launcher-emacs [options]
      -v, --version    Display version
      -h, --help       Display this help
      ")))
          (begin
            (apply
             (partial launcher-emacs #:profile profile)
             rest-args)
            )))))

(testsymb 'main)

(module-evaluated)
