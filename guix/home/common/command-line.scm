(define-module (command-line)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)       ; last
  #:use-module (guix monads)       ; with-monad
  #:use-module (utils)             ; partial
  #:use-module (tests)             ; test-type
  #:use-module (settings)          ; user
  #:export (handle-cli))

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (command-line) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

|#

(define m (module-name-for-logging))
(evaluating-module)

(define-exception-type &handle-cli-exception &exception make-handle-cli-exception handle-cli-exception?
  ;; (field-name field-accessor) ...
  (handle-cli-procedure handle-cli-exception-procedure))

;; TODO rename fun, exec-fun to symb-fun symb-exec-fun
(define* (handle-cli
          #:key (verbose #t) utility fun exec-fun params
          ;; #:allow-other-keys
          #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'.

Example:
(handle-cli
 #:verbose  #f
 #:utility  \"rgt4\"
 #:fun      'cli-command
 #:exec-fun 'exec-foreground
 #:params   \"rg --ignore-case --pretty --type=lisp --context=4\"
  '((\"/home/bost/scm-bin/rgt4\" \"flatpakxxx\")))
"
  (define f (format #f "~a [handle-cli]" m))
  (when verbose
    (format #t "~a #:verbose  ~a ; ~a\n" f (pr-str-with-quote verbose)  (test-type verbose))
    (format #t "~a #:utility  ~a ; ~a\n" f (pr-str-with-quote utility)  (test-type utility))
    (format #t "~a #:fun      ~a ; ~a\n" f (pr-str-with-quote fun)      (test-type fun))
    (format #t "~a #:exec-fun ~a ; ~a\n" f (pr-str-with-quote exec-fun) (test-type exec-fun))
    (format #t "~a #:params   ~a ; ~a\n" f (pr-str-with-quote params)   (test-type params))
    (format #t "~a   args     ~a ; ~a\n" f (pr-str-with-quote args)     (test-type args)))
  (let* [(elements (list #:verbose #:utility #:fun #:exec-fun #:params))
         (args (remove-all-elements args elements))
         (args (car args))

         ;; (value #t): a given option expects accept a value
         (option-spec `[(help       (single-char #\h) (value #f))
                        (version    (single-char #\v) (value #f))
                        (gx-dry-run (single-char #\d) (value #f))
                        (create-frame (single-char #\c) (value #f))
                        (rest-args                    (value #f))])

         ;; TODO isn't the #:stop-at-first-non-option swapped?
         (options (getopt-long args option-spec #:stop-at-first-non-option #t))
         ;; #f means that the expected value wasn't specified
         (val-help       (option-ref options 'help       #f))
         (val-version    (option-ref options 'version    #f))
         (val-gx-dry-run (option-ref options 'gx-dry-run #f))
         (val-create-frame (option-ref options 'create-frame #f))
         (val-rest-args  (option-ref options '()         #f))]
    (when verbose
      (format #t "~a option-spec    : ~a\n" f option-spec)
      (format #t "~a options        : ~a\n" f options)
      (format #t "~a val-help       : ~a\n" f val-help)
      (format #t "~a val-version    : ~a\n" f val-version)
      (format #t "~a val-gx-dry-run : ~a\n" f val-gx-dry-run)
      (format #t "~a val-create-frame : ~a\n" f val-create-frame)
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
      (let* [(emacs-procedures '(
                                 pkill-server
                                 create-launcher
                                 set-editable
                                 ))
             (procedures (append emacs-procedures
                                 '(
                                   cli-command
                                   cli-background-command
                                   cli-system-command
                                   mount unmount eject
                                   )))]
        (if (member? fun procedures)
            (let* [(fun-symbol
                    (if (member? fun
                                 '(
                                   cli-command
                                   cli-background-command
                                   cli-system-command)
                                 )
                        'cli-general-command
                        fun))
                   ;; resolve fun-symbol to a procedure
                   (procedure-fun (eval fun-symbol (current-module)))
                   (procedure-exec-fun (eval exec-fun (current-module)))
                   ]
              ;; (format #t "~a procedure-fun      : ~a\n" f procedure-fun)
              ;; (format #t "~a procedure-exec-fun : ~a\n" f procedure-exec-fun)
              (apply procedure-fun
                     (append
                      (list
                       #:utility    utility ; probably not needed
                       #:gx-dry-run val-gx-dry-run
                       #:verbose    verbose
                       #:params     params
                       )
                      (if (not (member? fun emacs-procedures))
                          (list
                           #:fun      fun
                           #:exec-fun procedure-exec-fun)
                          (list))
                      (if (equal? fun 'create-launcher)
                          (list #:create-frame val-create-frame)
                          (list))
                      val-rest-args)))
            ;; (let* [(fun-symbol
            ;;         (if (member? fun
            ;;                      '(
            ;;                        cli-command
            ;;                        cli-background-command
            ;;                        cli-system-command
            ;;                        ))
            ;;             'cli-general-command
            ;;             fun))
            ;;        ;; resolve fun-symbol to a procedure
            ;;        (procedure-fun (eval fun-symbol (current-module)))
            ;;        (procedure-exec-fun (eval exec-fun (current-module)))
            ;;        ]
            ;;   ;; (format #t "~a procedure-fun      : ~a\n" f procedure-fun)
            ;;   ;; (format #t "~a procedure-exec-fun : ~a\n" f procedure-exec-fun)
            ;;   ((comp
            ;;     (partial apply procedure-fun)
            ;;     (lambda (v) (format #t "~a 1. ~a\n" m v) v)
            ;;     (partial remove unspecified-or-empty-or-false?)
            ;;     (lambda (v) (format #t "~a 0. ~a\n" m v) v)
            ;;     )
            ;;    (append
            ;;     (when #t
            ;;       (list #:gx-dry-run   val-gx-dry-run
            ;;             #:verbose      verbose
            ;;             #:params       params))
            ;;     (when (not (member? fun emacs-procedures))
            ;;       (list #:fun          fun
            ;;             #:exec-fun     procedure-exec-fun))
            ;;     (when (equal? fun 'create-launcher)
            ;;       (list #:create-frame val-create-frame))
            ;;     val-rest-args)))

            (raise-exception
             (make-exception
              (make-handle-cli-exception fun)
              (make-exception-with-message
               (format #t "The procedure must be one of:\n  ~a\n\n"
                       procedures))))))])))
(testsymb 'handle-cli)

(module-evaluated)
