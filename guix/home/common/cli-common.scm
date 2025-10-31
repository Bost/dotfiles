(define-module (cli-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (dotf utils)
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (cli-common) -s
!#

cd $dotf
./guix/home/common/cli-common.scm 'define\* \(exec'
|#

(define m (module-name-for-logging))
(evaluating-module)

(def*-public (cli-general-command
              #:key (trace #f) verbose gx-dry-run params fun exec-fun
              #:allow-other-keys #:rest args)
  "The ARGS are being ignored.
TRACE - trace procedure parameters
VERBOSE - print command line of the command being executed on the CLI

Examples:
(cli-general-command
  #:gx-dry-run #t
  #:verbose #t
  #:params \"ls -la\" \"rest\" \"args\")

(cli-general-command
  #:params \"rg --pretty -t lisp\" \"define\\\\* \\\\(exec\")

(cli-general-command
  #:params \"ls -la\" \"rest\" \"args\")

(cli-general-command #:fun 'cli-system-command
                     #:exec-fun exec-system
                     #:params \"sudo /run/current-system/profile/sbin/reboot\")

(cli-general-command #:fun 'cli-system-command
                     #:exec-fun exec-system
                     #:params \"echo foo\")

(cli-general-command #:fun 'cli-background-command
                     #:exec-fun exec-background
                     #:params \"echo foo\")

(cli-general-command #:fun 'cli-command
                     #:exec-fun exec-foreground
                     #:params \"echo foo\")
"
  (when trace
    (format #t "~a trace      : ~s\n" f trace)
    (format #t "~a verbose    : ~s\n" f verbose)
    (format #t "~a gx-dry-run : ~s\n" f gx-dry-run)
    (format #t "~a params     : ~s\n" f params)
    (format #t "~a fun        : ~s\n" f fun)
    (format #t "~a exec-fun   : ~s\n" f exec-fun)
    (format #t "~a args       : ~s\n" f args))
  (let* [(elements (list #:trace #:verbose
                         #:gx-dry-run #:params #:fun #:exec-fun))
         (args (remove-all-elements args elements))]
    (if gx-dry-run
        (begin
          ;; <stdin>:1494:40: warning: possibly unbound variable `monad'
          ;; (format #t "~a monad: ~a\n" f monad)
          (format #t "~a TODO implement --gx-dry-run\n" f))
        (begin
          ((comp
            ;; (lambda (p) (format #t "~a done\n" f) p)
            ;; (partial apply (partial exec-fun #:verbose verbose params))
            (lambda (command) (exec-fun command #:verbose verbose))
            ;; (lambda (p) (format #t "~a 3. ~a\n" f p) p)

            ;; (lambda (lst) (when (equal? fun 'cli-system-command)
            ;;                 (dbg-exec params))
            ;;         lst)

            ;; (lambda (p) (format #t "~a 2. ~a\n" f p) p)
            (partial append (list params))
            ;; (lambda (p) (format #t "~a 1. ~a\n" f p) p)
            (partial map (partial format #f "~s"))
            ;; (lambda (p) (format #t "~a 0. ~a\n" f p) p)
            )
           args)))))

(def*-public (cli-command
              #:key (trace #f) verbose gx-dry-run params fun exec-fun
              #:allow-other-keys #:rest args)
  ((comp
    (partial apply cli-general-command)
    ;; (lambda (v) (format #t "~a 1 ~a\n" f v) v)
    ;; Need to append '#:trace trace' so that it's not redefined later on
    (partial append (list #:trace trace #:exec-fun exec-foreground))
    ;; (lambda (v) (format #t "~a 0 ~a\n" f v) v)
    )
   args))

(define*-public (cli-background-command
          #:key (trace #f) verbose gx-dry-run params fun exec-fun
          #:allow-other-keys #:rest args)
  ((comp
    (partial apply cli-general-command)
    ;; Need to append '#:trace trace' so that it's not redefined later on
    (partial append (list #:trace trace #:exec-fun exec-background)))
   args))

(define*-public (cli-system-command
          #:key (trace #f) verbose gx-dry-run params fun exec-fun
          #:allow-other-keys #:rest args)
  ((comp
    (partial apply cli-general-command)
    ;; Need to append '#:trace trace' so that it's not redefined later on
    (partial append (list #:trace trace #:exec-fun (partial apply system))))
   args))

(define-public (main . args)
  (format #t "~a\n" args)
  (format #t "car: ~a\n" (car args))
  (format #t "cdr: ~a\n" (cdr args))
  ;; (format #t "cadr: ~a\n" (cadr args)) ;; doesn't work
  (format #t "caar: ~a\n" (caar args))
  (format #t "cdar: ~a\n" (cdar args))
  (format #t "cadar: ~a\n" (cadar args))
  (cli-command #:verbose #t #:params "rg --pretty -t lisp" (cadar args)))

(module-evaluated)
