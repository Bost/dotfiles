(define-module (command-line)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (guix monads)       ; with-monad
  #:use-module (bost common utils)        ; partial
  #:use-module (bost common tests)        ; test-type
  #:use-module (dotf settings)     ; user
  #:use-module (cli-common)        ; for (eval ...) of cli-*command utility-funs
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (command-line) -s
!#

This module is not directly executed. No main-procedure is needed.
|#

(define m (module-name-for-logging))
(evaluating-module)

(define-exception-type
  &handle-cli-exception
  &exception
  make-handle-cli-exception
  handle-cli-exception?
  ;; (field-name field-accessor) ...
  (handle-cli-procedure handle-cli-exception-procedure))

;; TODO rename fun, exec-fun -> symb-fun symb-exec-fun
;; TODO rename verbose -> verbose-exec-fun
(def*-public (handle-cli #:key verbose utility fun exec-fun
                         params
                         profile ; for Emacs launchers
                         ignore-errors
                         #:allow-other-keys
                         #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'.

Examples:
(handle-cli
 #:verbose  #f
 #:utility  \"rgt4\"
 #:fun      'cli-general-command
 #:exec-fun 'exec-foreground
 #:params   \"rg --ignore-case --pretty --type=lisp --context=4\"
  '((\"/home/bost/scm-bin/rgt4\" \"flatpakxxx\")))

(handle-cli
 #:utility  \"techo\"
 #:fun      'cli-general-command
 #:exec-fun 'exec-background
 #:params   \"echo \\\"foo\\\"\")
"
  (trc "#:verbose:" verbose)
  (trc "#:utility:" utility)
  (trc "#:fun:" fun)
  (trc "#:exec-fun:" exec-fun)
  (trc "#:params:" params)
  (trc "#:profile:" profile)
  (trc "#:ignore-errors:" ignore-errors)
  (trc "args:" args)
  (let* [
         ;; Needed is e.g. '("/home/bost/scm-bin/f" "<file-name-pattern>")
         (command-line (last args))

         ;; (value #t): a given option expects accept a value
         (option-spec `[(help         (single-char #\h) (value #f))
                        (version      (single-char #\v) (value #f))
                        (gx-dry-run   (single-char #\d) (value #f))
                        (rest-args                      (value #f))])

         (options (getopt-long command-line option-spec
                               ;; Use in conjunction with #:allow-other-keys
                               ;; #:stop-at-first-non-option #t
                               ))
         ;; #f means that the expected value wasn't specified
         (val-help         (option-ref options 'help       #f))
         (val-version      (option-ref options 'version    #f))
         (val-gx-dry-run   (option-ref options 'gx-dry-run #f))
         (val-rest-args    (option-ref options '()         #f))]
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
      (let* [(emacs-procedures '(pkill-server create-launcher set-editable))
             (utility-funs (append emacs-procedures
                                 '(
                                   cli-general-command mount unmount eject info
                                   )))]
        (if (member? fun utility-funs)
            (let [(eval-here (lambda (exp) (eval exp
                                                 ;; (interaction-environment)
                                                 (current-module))))]
              (apply (eval-here fun)
                     (append
                      (list
                       #:verbose       verbose
                       #:gx-dry-run    val-gx-dry-run
                       #:ignore-errors ignore-errors
                       )
                      (if (member? fun emacs-procedures)
                          (list #:profile profile)
                          (list
                           #:params params
                           #:exec-fun (eval-here exec-fun)))
                      val-rest-args)))

            (raise-exception
             (make-exception
              (make-handle-cli-exception fun)
              (make-exception-with-message
               (format #t "The value of ~a is ~s. Expecting one of:\n  ~a\n\n"
                       #:fun fun utility-funs))))))])))
(testsymb 'handle-cli)

(module-evaluated)
