(define-module (cli-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (dotf utils)
  #:use-module (dotf tests)
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

;; TODO rename params -> cli-command
(def*-public (cli-general-command
              #:key (trace #f) verbose gx-dry-run params exec-fun ignore-errors
              #:allow-other-keys #:rest args)
  "The ARGS are being ignored.
TRACE - trace procedure parameters
VERBOSE - print command line of the command being executed on the CLI

Examples:
(cli-general-command #:gx-dry-run #t #:verbose #t
                     #:exec-fun exec-foreground
                     #:params \"ls -la\" \"rest\" \"args\")

(cli-general-command #:exec-fun exec-foreground
                     #:params \"rg --pretty -t lisp\" \"define\\\\* \\\\(exec\")

(cli-general-command #:exec-fun exec-foreground
                     #:params \"ls -la\" \"rest\" \"args\")

(cli-general-command #:exec-fun exec-system
                     #:params \"sudo /run/current-system/profile/sbin/reboot\")

(cli-general-command #:exec-fun exec-system     #:params \"echo foo\")
(cli-general-command #:exec-fun exec-background #:params \"echo foo\")
(cli-general-command #:exec-fun exec-foreground #:params \"echo foo\")
"
  (when trace
    (format #t "~a   args          ~a ; ~a\n" f (pr-str-with-quote args)          (test-type args))
    (format #t "~a #:trace         ~a ; ~a\n" f (pr-str-with-quote trace)         (test-type trace))
    (format #t "~a #:verbose       ~a ; ~a\n" f (pr-str-with-quote verbose)       (test-type verbose))
    (format #t "~a #:gx-dry-run    ~a ; ~a\n" f (pr-str-with-quote gx-dry-run)    (test-type gx-dry-run))
    (format #t "~a #:params        ~a ; ~a\n" f (pr-str-with-quote params)        (test-type params))
    (format #t "~a #:exec-fun      ~a ; ~a\n" f (pr-str-with-quote exec-fun)      (test-type exec-fun))
    (format #t "~a #:ignore-errors ~a ; ~a\n" f (pr-str-with-quote ignore-errors) (test-type ignore-errors))
    )
  ((comp
    ;; (lambda (p) (format #t "~a done\n" f) p)
    (lambda (command)
      (if gx-dry-run
          (begin
            ;; <stdin>:1494:40: warning: possibly unbound variable `monad'
            ;; (format #t "~a monad: ~a\n" f monad)
            (format #t "~a --gx-dry-run : ~a\n" f gx-dry-run)
            (format #t "~a ~a\n" f command))
          (exec-fun command
                    #:trace trace
                    #:verbose verbose
                    #:ignore-errors ignore-errors
                    )))
    ;; (lambda (p) (format #t "~a 3. ~a\n" f p) p)
    (partial append (list params))
    ;; (lambda (p) (format #t "~a 2. ~a\n" f p) p)
    (partial map (partial format #f "~s"))
    ;; (lambda (p) (format #t "~a 1. ~a\n" f p) p)
    ;; Extract the string typed on the command line
    (lambda (lst) (remove-all-elements
                   lst (list #:trace #:verbose #:gx-dry-run
                             #:params #:exec-fun #:ignore-errors)))
    ;; (lambda (p) (format #t "~a 0. ~a\n" f p) p)
    )
   args))

(define-public (main . args)
  (format #t "~a\n" args)
  (format #t "car: ~a\n" (car args))
  (format #t "cdr: ~a\n" (cdr args))
  ;; (format #t "cadr: ~a\n" (cadr args)) ;; doesn't work
  (format #t "caar: ~a\n" (caar args))
  (format #t "cdar: ~a\n" (cdar args))
  (format #t "cadar: ~a\n" (cadar args))
  (cli-general-command #:verbose #t
                       #:params "rg --pretty -t lisp" (cadar args)))

(module-evaluated)
