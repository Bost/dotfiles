(define-module (emacs-common)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils) ;; partial
  #:use-module (settings)
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (
            create-emacs-launcher
            handle-cli
            pkill-server
            set-config-editable
            ))

#|
;; -e calls the `main` function

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (emacs-common) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

|#

(define m (module-name-for-logging))
(evaluating-module)

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

(define* (pkill-server #:key utility-name gx-dry-run profile #:rest args)
  "
Usage:
(pkill-server #:gx-dry-run #t #:profile \"spacemacs\" \"rest\" \"args\")
(pkill-server #:gx-dry-run #t #:profile \"spguimacs\" \"rest\" \"args\")
(pkill-server                 #:profile \"spacemacs\" \"rest\" \"args\")
(pkill-server                 #:profile \"spguimacs\" \"rest\" \"args\")
"
  (let* [
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         ]
    ;; (format #t "~a gx-dry-run : ~a\n" m gx-dry-run)
    ;; (format #t "~a profile : ~a\n" m profile)
    ;; (format #t "~a args : ~a\n" m args)
    ;; pkill-pattern must NOT be eclosed by \"\"
    (let* [(pkill-pattern
            (format #f "~a --with-profile=~a --daemon"
                    (which-emacs) profile))]
      ;; TODO use with-monad
      (apply exec-system*-new
             #:split-whitespace #f
             #:gx-dry-run gx-dry-run
             "pkill" "--full" pkill-pattern args))))
(testsymb 'pkill-server)

(define* (create-emacs-launcher #:key utility-name gx-dry-run profile #:rest args)
  "
(create-emacs-launcher #:profile \"spacemacs\" \"rest\" \"args\")
"
  (let* [(f "[create-emacs-launcher]")
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         (emacs-bin (which-emacs))
         (init-cmd
          (cmd->string
           (append
            (list emacs-bin
                  (str "--with-profile=" profile)
                  ;; (str "--init-directory=$HOME/.emacs.d.distros/" profile)
                  "--daemon")
            ;; the init-cmd must not contain the args otherwise the `pgrep
            ;; ...` detection won't work
            #;args)))
         ]
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
     args)))
(testsymb 'create-emacs-launcher)

(define* (set-config-editable #:key utility-name gx-dry-run profile #:rest args)
  "
Usage:
(set-config-editable #:gx-dry-run #t #:profile \"spacemacs\" \"rest\" \"args\")
(set-config-editable #:gx-dry-run #t #:profile \"spguimacs\" \"rest\" \"args\")
(set-config-editable                 #:profile \"spacemacs\" \"rest\" \"args\")
(set-config-editable                 #:profile \"spguimacs\" \"rest\" \"args\")
"
  (let* [(f "[set-config-editable]")
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         (monad (if gx-dry-run
                    compose-commands-guix-shell-dry-run
                    compose-commands-guix-shell))]
    (if gx-dry-run
        (begin
          (format #t "~a ~m monad: ~a\n" m f monad)
          (format #t "~a ~m TODO implement --gx-dry-run\n" m f))
        (let* [(profile-kw (cond
                            [(string= profile spguimacs) #:spguimacs]
                            [(string= profile spacemacs) #:spacemacs]))
               (dst (get-cfg profile-kw))
               (src (get-src profile-kw))]
          (with-monad monad
            (>>=
             (return (list dst))
             mdelete-file
             `(override-mv ,src ,dst)
             mcopy-file))
          (copy-file src dst)))))
(testsymb 'set-config-editable)

(define* (handle-cli #:key (dbg #f) utility-name fun profile #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so that
the options-parser doesn't complain about e.g. 'no such option: -p'."
  ;; (format #t "~a args: ~a\n" m args)
  (let* [(f "[handle-cli]")
         (args (remove-kw-from-args #:dbg args))
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:fun args))
         (args (remove-kw-from-args #:profile args))
         (args (car args))

          ;; (value #t): a given option expects accept a value
         (option-spec `[
                        (help       (single-char #\h) (value #f))
                        (version    (single-char #\v) (value #f))
                        (gx-dry-run (single-char #\d) (value #f))
                        (rest-args                    (value #f))])

         ;; TODO isn't the #:stop-at-first-non-option swapped?
         (options (getopt-long args option-spec #:stop-at-first-non-option #t))
         ;; #f means that the expected value wasn't specified
         (val-help       (option-ref options 'help       #f))
         (val-version    (option-ref options 'version    #f))
         (val-gx-dry-run (option-ref options 'gx-dry-run #f))
         (val-rest-args  (option-ref options '()         #f))
         ]
    (when dbg
      (format #t "~a ~a option-spec    : ~a\n" m f option-spec)
      (format #t "~a ~a options        : ~a\n" m f options)
      (format #t "~a ~a val-help       : ~a\n" m f val-help)
      (format #t "~a ~a val-version    : ~a\n" m f val-version)
      (format #t "~a ~a val-gx-dry-run : ~a\n" m f val-gx-dry-run)
      (format #t "~a ~a val-rest-args  : ~a\n" m f val-rest-args))
    (cond
     [val-help
      (format #t "~a [options]\n~a\n~a\n\n"
              utility-name
              "    -v, --version    Display version"
              "    -h, --help       Display this help")]
     [val-version
      (format #t "~a version <...>\n" utility-name)]
     [#t
      (apply (partial fun
                      #:utility-name utility-name
                      #:gx-dry-run val-gx-dry-run
                      #:profile profile)
             val-rest-args)])))
(testsymb 'handle-cli)

(module-evaluated)
