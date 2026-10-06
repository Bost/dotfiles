(define-module (emacs-common)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (guix monads)       ; with-monad
  #:use-module (bost common utils)        ; partial
  #:use-module (bost common tests)        ; test-type
  #:use-module (dotf settings)     ; user
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (ice-9 optargs)     ; define*-public
  #:use-module (dotf fs-utils)     ; user-home dev user-dev dotf user-dotf
  )

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (emacs-common) -s
!#

This module is not directly executed. No main-procedure is needed.
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
(create-init-cmd \"spgx\") ; =>
\"emacs --init-directory=/gnu/store/...-emacs-spacemacs-.../share/emacs/site-lisp/spacemacs-... --bg-daemon=spgx\"

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

(def*-public (pkill-server
              #:key (verbose #f) (ignore-errors #f)
              utility gx-dry-run profile
              #:rest args)
  "The ARGS are being ignored.

Usage:
(pkill-server #:gx-dry-run #t #:profile \"develop\" \"rest\" \"args\")
(pkill-server #:gx-dry-run #t #:profile \"guix\"    \"rest\" \"args\")
(pkill-server                 #:profile \"develop\" \"rest\" \"args\")
(pkill-server                 #:profile \"guix\"    \"rest\" \"args\")
"
  (trc "args:" args)
  (trc "#:verbose:" verbose)
  (trc "#:ignore-errors:" ignore-errors)
  (trc "#:utility:" utility)
  (trc "#:gx-dry-run:" gx-dry-run)
  (trc "#:profile:" profile)

  (let* [(elements (list #:verbose #:ignore-errors
                         #:utility #:gx-dry-run #:profile))
         (filtered-args (remove-all-elements args elements))]
    ;; pkill-pattern must NOT be enclosed by \"\"
    ;; TODO use with-monad
    (apply exec-system*-new
           #:split-whitespace #f
           #:gx-dry-run gx-dry-run
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
              #:key (verbose #f) (ignore-errors #f)
              utility gx-dry-run profile
;;; By not allowing other keys I don't have to remove them later on
              #:allow-other-keys
              #:rest args)
  "Uses `user' from settings. The ARGS are used only when `emacsclient' command
 is executed. The server, called by `emacs' ignores them.
Tracing is controlled by tracing-enabled? and tracing-procedures.
VERBOSE - print command line of the command being executed on the CLI

TODO create-launcher ignores servers with '--debug-init' in the init-cmd.

Examples:
(create-launcher #:profile \"develop\" \"rest\" \"args\")

(create-launcher #:profile \"guix\"
                 \"guix/home/common/cli-common.scm\")

(create-launcher #:profile \"spgx\"
                 \"guix/home/common/cli-common.scm\")
"
  (trc "args:" args)
  (trc "#:verbose:" verbose)
  (trc "#:ignore-errors:" ignore-errors)
  (trc "#:utility:" utility)
  (trc "#:gx-dry-run:" gx-dry-run)
  (trc "#:profile:" profile)

  (let* [(elements (list #:verbose #:ignore-errors
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
                         [(and #f (string= profile spgx))
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

(define (make-pair-dst-src profile)
  (cons (str (get-cfg profile) "/" emacs-init-file)
        ((comp
          (lambda (path)
            (user-dotf emacs-distros path "/" emacs-init-file))
          (partial substring (get-cfg profile)))
         (string-length (user-home emacs-distros)))))

(def*-public (set-editable
              #:key (verbose #f) (ignore-errors #f)
              utility gx-dry-run profile
              #:rest args)
  "The ARGS are being ignored.
Tracing is controlled by tracing-enabled? and tracing-procedures.
VERBOSE - print command line of the command being executed on the CLI

Examples:
(set-editable #:gx-dry-run #t #:profile \"develop\" \"rest\" \"args\")
(set-editable #:gx-dry-run #t #:profile \"guix\"    \"rest\" \"args\")
(set-editable                 #:profile \"develop\" \"rest\" \"args\")
(set-editable                 #:profile \"guix\"    \"rest\" \"args\")
"
  (trc "args:" args)
  (trc "#:verbose:" verbose)
  (trc "#:ignore-errors:" ignore-errors)
  (trc "#:utility:" utility)
  (trc "#:gx-dry-run:" gx-dry-run)
  (trc "#:profile:" profile)

  (let* [(elements (list #:verbose #:ignore-errors
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

(def* (handle-cli #:key verbose utility fun profile
                  #:allow-other-keys
                  #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'.
Tracing is controlled by tracing-enabled? and tracing-procedures.
VERBOSE - print command line of the command being executed on the CLI

(begin
  (use-modules (ice-9 getopt-long) (ice-9 regex) (guix monads)
               (bost common srfi-1-smart) (bost common utils) (bost common tests) (dotf settings)
               (cli-common) (command-line) (emacs-common))
  (handle-cli #:verbose #t #:exec-fun 'exec-foreground
              #:utility \"s\" #:fun 'create-launcher #:profile \"spgx\"
              (command-line)))

(handle-cli
   #:verbose #t #:exec-fun 'exec-foreground
   #:utility \"s\" #:fun create-launcher #:profile \"spgx\"
   (list \"\" \"guix/home/common/cli-common.scm\"))
"
  (trc "verbose:" verbose)
  (trc "utility:" utility)
  (trc "fun:" fun)
  (trc "profile:" profile)
  (trc "args:" args)
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
    (trc "option-spec:" option-spec)
    (trc "options:" options)
    (trc "val-help:" val-help)
    (trc "val-version:" val-version)
    (trc "val-gx-dry-run:" val-gx-dry-run)
    (trc "val-rest-args:" val-rest-args)
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
                      #:verbose verbose
                      #:utility utility
                      #:gx-dry-run val-gx-dry-run
                      #:profile profile)
             val-rest-args)])))
(testsymb 'handle-cli)

(module-evaluated)
