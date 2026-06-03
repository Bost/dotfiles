(define-module (emacs-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (guix monads)       ; with-monad
  #:use-module (dotf utils)        ; partial
  #:use-module (dotf tests)        ; test-type
  #:use-module (dotf settings)     ; user
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (emacs-common) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (emacs-binary-path)
  "(emacs-binary-path)
=> \"/gnu/store/09a50cl6ndln4nmp56nsdvn61jgz2m07-emacs-29.1/bin/emacs\""
  ((comp
    (partial format #f "~a/bin/emacs")
    ;; WTF?!? In the REPL I get: Unbound variable: package-output-path
    package-output-path)
   (@(gnu packages emacs) emacs)))

(define-public (which-emacs)
  ;; (emacs-binary-path)

  ;; \"/home/bost/.guix-home/profile/bin/emacs\"
  ;; ((@(guix build utils) which) "emacs")

  "emacs")

(define (which-emacsclient)
  ;; "/home/bost/.guix-home/profile/bin/emacs"
  ;; ((@(guix build utils) which) "emacsclient")

  "emacsclient")

(define (calculate-socket profile)
  (when profile
    ((comp keyword->string cdr)
     (assoc profile profile->branch-kw))))

(define (create-init-cmd profile)
"
(create-init-cmd \"spguix\") ; =>
\"emacs --init-directory=/gnu/store/...-emacs-spacemacs-.../share/emacs/site-lisp/spacemacs-... --bg-daemon=spguix\"

(create-init-cmd \"guix\")    ; =>
\"emacs --init-directory=/home/bost/.emacs.d.distros/spacemacs/guix/src --bg-daemon=guix\"

(create-init-cmd \"crafted\") ; =>
\"emacs --init-directory=/home/bost/.emacs.d.distros/crafted-emacs --bg-daemon=crafted\"
"
  (cmd->string
   (list
    (which-emacs)
    (format #f "--init-directory=~a" (get-src profile))
    (str "--bg-daemon=" (calculate-socket profile)))))

(define (fmt s f prm) (format #t s f (pr-str-with-quote prm) (test-type prm)))

(def*-public (pkill-server
              #:key (trace #f) (verbose #f) (ignore-errors #f)
              utility gx-dry-run profile
              #:rest args)
  "The ARGS are being ignored.

Usage:
(pkill-server #:gx-dry-run #t #:profile \"develop\" \"rest\" \"args\")
(pkill-server #:gx-dry-run #t #:profile \"guix\"    \"rest\" \"args\")
(pkill-server                 #:profile \"develop\" \"rest\" \"args\")
(pkill-server                 #:profile \"guix\"    \"rest\" \"args\")
"
  (when trace
    (fmt "~a   args          ~a ; ~a\n" f args)
    (fmt "~a #:trace         ~a ; ~a\n" f trace)
    (fmt "~a #:verbose       ~a ; ~a\n" f verbose)
    (fmt "~a #:ignore-errors ~a : ~a\n" f ignore-errors)
    (fmt "~a #:utility       ~a ; ~a\n" f utility)
    (fmt "~a #:gx-dry-run    ~a ; ~a\n" f gx-dry-run)
    (fmt "~a #:profile       ~a ; ~a\n" f profile)
    )

  (let* [(elements (list #:trace #:verbose #:ignore-errors
                         #:utility #:gx-dry-run #:profile))
         (filtered-args (remove-all-elements args elements))]
    ;; pkill-pattern must NOT be enclosed by \"\"
    ;; TODO use with-monad
    (apply exec-system*-new
           #:split-whitespace #f
           #:gx-dry-run gx-dry-run
           #:trace trace
           #:verbose verbose
           (list "pkill" "--echo" "--full" (create-init-cmd profile)))))
(testsymb 'pkill-server)

(define (eval-crafted)
  (format #f (str "--eval='(message \" CRAFTED_EMACS_HOME : %s\" "
                  "(getenv \"CRAFTED_EMACS_HOME\"))'")))

(define (eval-spacemacs)
  (format #f (str "--eval='(message \" SPACEMACSDIR : %s\\n "
                  "dotspacemacs-directory : %s\\n "
                  "dotspacemacs-server-socket-dir : %s\" "
                  "(getenv \"SPACEMACSDIR\") dotspacemacs-directory "
                  "dotspacemacs-server-socket-dir)'")))

(define (eval-xdata-home profile)
  (format #f (str "--eval '(progn
  (setq user-emacs-directory (concat (getenv \"XDG_DATA_HOME\") \"/spacemacs/~a/\"))
)'") profile))

(define (init-cmd-env-vars home-emacs-distros profile)
  (if (string= profile crafted)
      (format #f "CRAFTED_EMACS_HOME=~a/crafted-emacs/personal"
              home-emacs-distros)
      (format #f "SPACEMACSDIR=~a/spacemacs/~a/cfg"
              home-emacs-distros profile)))

(def*-public (create-launcher
              #:key (trace #f) (verbose #f) (ignore-errors #f)
              utility gx-dry-run profile
;;; By not allowing other keys I don't have to remove them later on
              #:allow-other-keys
              #:rest args)
  "Uses `user' from settings. The ARGS are used only when `emacsclient' command
 is executed. The server, called by `emacs' ignores them.
TRACE - trace procedure parameters
VERBOSE - print command line of the command being executed on the CLI

TODO create-launcher ignores servers with '--debug-init' in the init-cmd.

Examples:
(create-launcher #:profile \"develop\" \"rest\" \"args\")

(create-launcher #:profile \"guix\"
                 \"guix/home/common/cli-common.scm\")

(create-launcher #:profile \"spguix\"
                 \"guix/home/common/cli-common.scm\")
"
  (when trace
    (fmt "~a   args          ~a ; ~a\n" f args)
    (fmt "~a #:trace         ~a ; ~a\n" f trace)
    (fmt "~a #:verbose       ~a ; ~a\n" f verbose)
    (fmt "~a #:ignore-errors ~a : ~a\n" f ignore-errors)
    (fmt "~a #:utility       ~a ; ~a\n" f utility)
    (fmt "~a #:gx-dry-run    ~a ; ~a\n" f gx-dry-run)
    (fmt "~a #:profile       ~a ; ~a\n" f profile)
    )

  (let* [(elements (list #:trace #:verbose #:ignore-errors
                         #:utility #:gx-dry-run #:profile))
         (filtered-args (remove-all-elements args elements))
         (init-cmd (create-init-cmd profile))]
    ((comp
;;; Search for the full command line:
;;; $ pkill --full /home/bost/.guix-profile/bin/emacs --init-directory=/home/bost/.emacs.d.distros/crafted-emacs --bg-daemon=crafted
      (lambda (client-cmd)
        (let* [(results
                ((comp
                  ;; (lambda (v) (format #t "~a 3. ~a\n" f v) v)
                  (lambda (client-cmd?)
                    ;; Put the values in a property list for debugging purposes
                    (list
                     #:client-cmd? client-cmd?
                     #:cmd-with-args
                     (append
                      client-cmd
                      (list "--reuse-frame" "--no-wait")
                      (if (null? filtered-args) '("./") filtered-args))))
                  ;; (lambda (v) (format #t "~a 1. ~a\n" f v) v)
                  (partial equal? client-cmd)
                  ;; (lambda (v) (format #t "~a 0. ~a\n" f v) v)
                  )
                 (compute-cmd
                  #:user user
                  #:init-cmd init-cmd
                  #:client-cmd client-cmd
                  #:pgrep-pattern init-cmd)))
               (client-cmd?   (plist-get results #:client-cmd?))
               (cmd-with-args (plist-get results #:cmd-with-args))]
          ;; (format #t "~a client-cmd?   : ~a\n" f client-cmd?)
          ;; (format #t "~a cmd-with-args : ~a\n" f cmd-with-args)

          (if client-cmd?
              (exec-background cmd-with-args #:verbose #t)
              (let* [(cmd-result-struct
                      (exec
                       ;; Only the initial command needs to be executed in a
                       ;; modified environment
                       (append
                        (list (init-cmd-env-vars home-emacs-distros profile) init-cmd)
                        (cond
                         [(and #f (string= profile crafted)) (list (eval-crafted))]
                         [(and #f (string= profile spguix))
                          ;; (eval-spacemacs)
                          (list
                           "--debug-init"
                           ;; (eval-xdata-home profile)
                           )]
                         [#t (list)]))
                       #:return-plist #t))
                     (retcode (plist-get cmd-result-struct #:retcode))]
                (when (zero? retcode)
                  ;; Calling (exec-background cmd-with-args) makes sense
                  ;; only if the Emacs server has been started successfully.
                  (exec-background cmd-with-args #:verbose #t)))))))
     (list (which-emacsclient)
           (str "--socket-name=" (calculate-socket profile))))))
(testsymb 'create-launcher)

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

(def*-public (set-editable
              #:key (trace #f) (verbose #f) (ignore-errors #f)
              utility gx-dry-run profile
              #:rest args)
  "The ARGS are being ignored.
TRACE - trace procedure parameters
VERBOSE - print command line of the command being executed on the CLI

Examples:
(set-editable #:gx-dry-run #t #:profile \"develop\" \"rest\" \"args\")
(set-editable #:gx-dry-run #t #:profile \"guix\"    \"rest\" \"args\")
(set-editable                 #:profile \"develop\" \"rest\" \"args\")
(set-editable                 #:profile \"guix\"    \"rest\" \"args\")
"
  (when trace
    (fmt "~a   args          ~a ; ~a\n" f args)
    (fmt "~a #:trace         ~a ; ~a\n" f trace)
    (fmt "~a #:verbose       ~a ; ~a\n" f verbose)
    (fmt "~a #:ignore-errors ~a : ~a\n" f ignore-errors)
    (fmt "~a #:utility       ~a ; ~a\n" f utility)
    (fmt "~a #:gx-dry-run    ~a ; ~a\n" f gx-dry-run)
    (fmt "~a #:profile       ~a ; ~a\n" f profile)
    )

  (let* [(elements (list #:trace #:verbose #:ignore-errors
                         #:utility #:gx-dry-run #:profile))
         (args (remove-all-elements args elements))

         (monad (if gx-dry-run
                    compose-commands-guix-shell-dry-run
                    compose-commands-guix-shell))]
    (if gx-dry-run
        (begin
          (format #t "~a monad: ~a\n" f monad)
          (format #t "~a TODO implement --gx-dry-run\n" f))
        (let* [(dst-src (make-pair-dst-src profile))
               (dst (car dst-src))
               (src (cdr dst-src))]
          (with-monad monad
            (>>=
             (return (list dst))
             mdelete-file
             `(override-mv ,src ,dst)
             mcopy-file))
          (copy-file src dst)))))
(testsymb 'set-editable)

(def* (handle-cli #:key (trace #f) verbose utility fun profile
                  #:allow-other-keys
                  #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'.
TRACE - trace procedure parameters
VERBOSE - print command line of the command being executed on the CLI

(begin
  (use-modules (ice-9 getopt-long) (ice-9 regex) (guix monads)
               (dotf srfi-1-smart) (dotf utils) (dotf tests) (dotf settings)
               (cli-common) (command-line) (emacs-common))
  (handle-cli #:trace #f #:verbose #t #:exec-fun 'exec-foreground
              #:utility \"s\" #:fun 'create-launcher #:profile \"spguix\"
              (command-line)))

(handle-cli
   #:trace #t
   #:verbose #t #:exec-fun 'exec-foreground
   #:utility \"s\" #:fun create-launcher #:profile \"spguix\"
   (list \"\" \"guix/home/common/cli-common.scm\"))
"
  (when trace
    (format #t "~a trace   : ~a\n" f trace)
    (format #t "~a verbose : ~a\n" f verbose)
    (format #t "~a utility : ~a\n" f utility)
    (format #t "~a fun     : ~a\n" f fun)
    (format #t "~a profile : ~a\n" f profile)
    (format #t "~a args    : ~a\n" f args))
  (let* [
         ;; Needed is e.g. '("/home/bost/scm-bin/g" "/path/to/file.ext")
         (command-line (last args))

         ;; (value #t): a given option expects accept a value
         (option-spec `[(help       (single-char #\h) (value #f))
                        (version    (single-char #\v) (value #f))
                        (gx-dry-run (single-char #\d) (value #f))
                        (rest-args                    (value #f))])

         (options (getopt-long command-line option-spec
                               ;; Use in conjunction with #:allow-other-keys
                               ;; #:stop-at-first-non-option #t
                               ))
         ;; #f means that the expected value wasn't specified
         (val-help       (option-ref options 'help       #f))
         (val-version    (option-ref options 'version    #f))
         (val-gx-dry-run (option-ref options 'gx-dry-run #f))
         (val-rest-args  (option-ref options '()         #f))]
    (when trace
      (format #t "~a option-spec    : ~a\n" f option-spec)
      (format #t "~a options        : ~a\n" f options)
      (format #t "~a val-help       : ~a\n" f val-help)
      (format #t "~a val-version    : ~a\n" f val-version)
      (format #t "~a val-gx-dry-run : ~a\n" f val-gx-dry-run)
      (format #t "~a val-rest-args  : ~a\n" f val-rest-args))
    (cond
     [val-help
      (format #t "~a [options]\n~a\n~a\n\n"
              utility
              "    -v, --version    Display version"
              "    -h, --help       Display this help")]
     [val-version
      (format #t "~a version <...>\n" utility)]
     [#t
      (apply (partial fun
                      #:trace trace
                      #:verbose verbose
                      #:utility utility
                      #:gx-dry-run val-gx-dry-run
                      #:profile profile)
             val-rest-args)])))
(testsymb 'handle-cli)

(module-evaluated)
