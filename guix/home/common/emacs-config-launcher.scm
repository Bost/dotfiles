(define-module (emacs-config-launcher)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils) ;; partial
  #:use-module (settings)

  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (handle-cli create-emacs-launcher))

#|
;; -e calls the `main` function

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (emacs-config-launcher) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

|#

(define m (module-name-for-logging))
(evaluating-module)

(define dbg #f)
(define utility-name (last (module-name (current-module))))

(define (emacs-binary-path)
  "(emacs-binary-path)
=> \"/gnu/store/09a50cl6ndln4nmp56nsdvn61jgz2m07-emacs-29.1/bin/emacs\""
  ((comp
    (partial format #f "~a/bin/emacs")
    package-output-path)
   (@(gnu packages emacs) emacs)))

(define-public (which-emacs)
  "(which-emacs) => \"/home/bost/.guix-home/profile/bin/emacs\""
  ;; (emacs-binary-path)
  ((@(guix build utils) which) "emacs"))

(define (which-emacsclient)
  "(which-emacsclient) => \"/home/bost/.guix-home/profile/bin/emacsclient\""
  ((@(guix build utils) which) "emacsclient"))

(define* (create-emacs-launcher #:key profile #:rest args)
  "
(create-emacs-launcher #:profile \"spacemacs\" \"rest\" \"args\")
"
  (let* [(args (remove-kw-from-args #:profile args))]
    ;; (format #t "~a profile : ~a; args : \n" m profile args)
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
(testsymb 'create-emacs-launcher)

(define* (handle-cli #:key utility-name fun #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so that
the options-parser doesn't complain about e.g. 'no such option: -p'."
  ;; (format #t "~a args: ~a\n" m args)
  (let* [
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:fun args))
         (args (car args))
         ]
    (let* [(option-spec
            '[
              (help       (single-char #\h))
              (version    (single-char #\v))
              ])]
      ;; TODO isn't the #:stop-at-first-non-option swapped?
      (let* [(options (getopt-long args option-spec #:stop-at-first-non-option #t))
             ;; #f means that the expected value wasn't specified
             (val-help       (option-ref options 'help    #f))
             (val-version    (option-ref options 'version #f))]
        (when dbg
          (format #t "~a option-spec   : ~a\n" m option-spec)
          (format #t "~a val-help      : ~a\n" m val-help)
          #;(format #t "~a val-version   : ~a\n" m val-version))
        (cond
         [val-help
          (format #t "~a [options]\n~a\n~a\n\n"
                  utility-name
                  "    -v, --version    Display version"
                  "    -h, --help       Display this help")]
         #;[val-version
          (format #t "~a version 1.23
" utility-name)]
         [#t
          (fun args)])))))
(testsymb 'handle-cli)

(module-evaluated)
