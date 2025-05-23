(define-module (emacs-common)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (guix monads)             #| with-monad   |#
  #:use-module (utils)                   #| partial      |#
  #:use-module (settings)                #| user         |#
  #:export (
            create-emacs-launcher
            handle-cli
            pkill-server
            set-editable
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

(define (create-init-cmd profile socket)
  (cmd->string
   (list
    (which-emacs)
    (if (string= profile crafted)
        (format #f "--init-directory=~a/crafted-emacs" home-emacs-distros)
        (format #f "--init-directory=~a/spacemacs/~a/src" home-emacs-distros profile))
    (str "--bg-daemon=" socket))))

(define* (pkill-server
          #:key (verbose #f) utility-name gx-dry-run profile socket
          #:rest args)
  "The ARGS are being ignored.

Usage:
(pkill-server #:gx-dry-run #t #:profile \"develop\" \"rest\" \"args\")
(pkill-server #:gx-dry-run #t #:profile \"guix\" \"rest\" \"args\")
(pkill-server                 #:profile \"develop\" \"rest\" \"args\")
(pkill-server                 #:profile \"guix\" \"rest\" \"args\")
"
  (let* [(f "[pkill-server]")]
    (when verbose
      (format #t "~a ~a utility-name : ~a\n" m f utility-name)
      (format #t "~a ~a gx-dry-run   : ~a\n" m f gx-dry-run)
      (format #t "~a ~a profile      : ~a\n" m f profile)
      (format #t "~a ~a socket       : ~a\n" m f socket)
      (format #t "~a ~a args         : ~a\n" m f args)))
  (let* [(args (remove-kw-from-args #:verbose args))
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         (args (remove-kw-from-args #:socket args))]
    ;; pkill-pattern must NOT be enclosed by \"\"
;; TODO use with-monad
    (apply exec-system*-new
           #:split-whitespace #f
           #:gx-dry-run gx-dry-run
           (list "pkill" "--full" (create-init-cmd profile socket)))))
(testsymb 'pkill-server)

(define (init-cmd-env-vars home-emacs-distros profile)
  (if (string= profile crafted)
      (format #f "CRAFTED_EMACS_HOME=~a/crafted-emacs/personal" home-emacs-distros)
      (format #f "SPACEMACSDIR=~a/spacemacs/~a/cfg" home-emacs-distros profile)))

(define* (create-emacs-launcher
          #:key (verbose #f) utility-name gx-dry-run profile socket
          #:rest args)
  "Uses `user' from settings. The ARGS are used only when `emacsclient' command
 is executed. The server, called by `emacs' ignores them.

Example:
(create-emacs-launcher #:profile \"develop\" \"rest\" \"args\")
"
  (let* [(f "[create-emacs-launcher]")]
    (when verbose
      (format #t "~a ~a utility-name : ~a\n" m f utility-name)
      (format #t "~a ~a gx-dry-run   : ~a\n" m f gx-dry-run)
      (format #t "~a ~a profile      : ~a\n" m f profile)
      (format #t "~a ~a socket       : ~a\n" m f socket)
      (format #t "~a ~a args         : ~a\n" m f args))
    (let* [(args (remove-kw-from-args #:verbose args))
           (args (remove-kw-from-args #:utility-name args))
           (args (remove-kw-from-args #:gx-dry-run args))
           (args (remove-kw-from-args #:profile args))
           (args (remove-kw-from-args #:socket args))
           (init-cmd (create-init-cmd profile socket))]
      ((comp
;;; Search for the full command line:
;;; $ pkill --full /home/bost/.guix-profile/bin/emacs --init-directory=/home/bost/.emacs.d.distros/crafted-emacs --bg-daemon=crafted
        (lambda (client-cmd)
          (let* [(client-cmd-with-args (append
                                        (list client-cmd)
                                        (if (null? args) '("./") args)))]
            (if (string=? client-cmd
                          (compute-cmd
                           #:user user
                           #:init-cmd init-cmd
                           #:client-cmd client-cmd
                           #:pgrep-pattern init-cmd))
                (exec-background client-cmd-with-args)
                (when ((comp zero? car exec)
                       ;; Only the initial command needs to be executed in a
                       ;; modified environment
                       (list (init-cmd-env-vars home-emacs-distros profile) init-cmd
                             ;; (if (string= profile crafted)
                             ;;     (format #f "--eval='(message \" CRAFTED_EMACS_HOME : %s\" (getenv \"CRAFTED_EMACS_HOME\"))'")
                             ;;     (format #f "--eval='(message \" SPACEMACSDIR : %s\\n dotspacemacs-directory : %s\\n dotspacemacs-server-socket-dir : %s\" (getenv \"SPACEMACSDIR\") dotspacemacs-directory dotspacemacs-server-socket-dir)'"))
                             ))
                  ;; Calling (exec-background client-cmd-with-args) makes sense
                  ;; only if the Emacs server has been started successfully.
                  (exec-background client-cmd-with-args)))))
        cmd->string)
       (list (which-emacsclient) "--create-frame"
             (str "--socket-name=" socket))))))
(testsymb 'create-emacs-launcher)

;; ### BEG: from (fs-utils)
(define* (user-home #:rest args) (apply str home args))
(define dev (user-home "/dev"))
(define* (user-dev #:rest args)  (apply str dev args))
(define dotf (user-dev "/dotfiles"))
(define* (user-dotf #:rest args) (apply str dotf args))
;; ### END: from (fs-utils)

(define (make-pair-dst-src profile)
  (cons (str (get-cfg profile) "/" emacs-init-file)
        ((comp
          (lambda (path)
            (user-dotf emacs-distros path "/" emacs-init-file))
          (partial substring (get-cfg profile)))
         (string-length (user-home emacs-distros)))))

(define* (set-editable
          #:key (verbose #f) utility-name gx-dry-run profile socket
          #:rest args)
  "The ARGS are being ignored.

Examples:
(set-editable #:gx-dry-run #t #:profile \"develop\" \"rest\" \"args\")
(set-editable #:gx-dry-run #t #:profile \"guix\" \"rest\" \"args\")
(set-editable                 #:profile \"develop\" \"rest\" \"args\")
(set-editable                 #:profile \"guix\" \"rest\" \"args\")
"
  (let* [(f "[set-editable]")]
    (when verbose
      (format #t "~a ~a utility-name : ~a\n" m f utility-name)
      (format #t "~a ~a gx-dry-run   : ~a\n" m f gx-dry-run)
      (format #t "~a ~a profile      : ~a\n" m f profile)
      (format #t "~a ~a socket       : ~a\n" m f socket)
      (format #t "~a ~a args         : ~a\n" m f args))
    (let* [(args (remove-kw-from-args #:verbose args))
           (args (remove-kw-from-args #:utility-name args))
           (args (remove-kw-from-args #:gx-dry-run args))
           (args (remove-kw-from-args #:profile args))
           (args (remove-kw-from-args #:socket args))

           (monad (if gx-dry-run
                      compose-commands-guix-shell-dry-run
                      compose-commands-guix-shell))]
      (if gx-dry-run
          (begin
            (format #t "~a ~a monad: ~a\n" m f monad)
            (format #t "~a ~a TODO implement --gx-dry-run\n" m f))
          (let* [(dst-src (make-pair-dst-src profile))
                 (dst (car dst-src))
                 (src (cdr dst-src))]
            (with-monad monad
              (>>=
               (return (list dst))
               mdelete-file
               `(override-mv ,src ,dst)
               mcopy-file))
            (copy-file src dst))))))
(testsymb 'set-editable)

(define* (handle-cli #:key (verbose #f) utility-name fun profile #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'."
  (let* [(f "[handle-cli]")]
    (when verbose
      (format #t "~a ~a utility-name : ~a\n" m f utility-name)
      (format #t "~a ~a fun          : ~a\n" m f fun)
      (format #t "~a ~a profile      : ~a\n" m f profile)
      (format #t "~a ~a args         : ~a\n" m f args))
    (let* [(args (remove-kw-from-args #:verbose args))
           (args (remove-kw-from-args #:utility-name args))
           (args (remove-kw-from-args #:fun args))
           (args (remove-kw-from-args #:profile args))
           (args (car args))

           ;; (value #t): a given option expects accept a value
           (option-spec `[(help       (single-char #\h) (value #f))
                          (version    (single-char #\v) (value #f))
                          (gx-dry-run (single-char #\d) (value #f))
                          (rest-args                    (value #f))])

           ;; TODO isn't the #:stop-at-first-non-option swapped?
           (options (getopt-long args option-spec #:stop-at-first-non-option #t))
           ;; #f means that the expected value wasn't specified
           (val-help       (option-ref options 'help       #f))
           (val-version    (option-ref options 'version    #f))
           (val-gx-dry-run (option-ref options 'gx-dry-run #f))
           (val-rest-args  (option-ref options '()         #f))]
      (when verbose
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
                        #:verbose verbose
                        #:utility-name utility-name
                        #:gx-dry-run val-gx-dry-run
                        #:profile profile
                        #:socket ;; i.e. daemon / server name
                        (when profile
                          (let* [(branch-kw (cdr (assoc profile
                                                        profile->branch-kw)))]
                            (keyword->string branch-kw))))
               val-rest-args)]))))
(testsymb 'handle-cli)

(module-evaluated)
