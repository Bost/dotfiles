(define-module (cli-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (utils)
  #:export (exec-full-command cli-command))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (cli-common) -s
!#

cd $dotf
./guix/home/common/cli-common.scm 'define\* \(exec'
|#


(define m (module-name-for-logging))
(evaluating-module)

(define* (exec-full-command full-command)
  ;; (apply exec-system* full-command)
  (let* [(ret (exec full-command))]
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          ;; process output
          (map (partial format #t "~a\n") output)
          ret)
        (begin
          (error-command-failed m)
          *unspecified*))))

(define* (cli-command
          #:key (verbose #f) utility gx-dry-run params
          #:rest args)
  "The ARGS are being ignored.
Examples:
(cli-command #:gx-dry-run #t #:params \"ls -la\" \"rest\" \"args\")
(cli-command                 #:params \"ls -la\" \"rest\" \"args\")

(cli-command #:verbose #t #:params \"rg --pretty -t lisp\" \"define\\\\* \\\\(exec\")
(cli-command              #:params \"rg --pretty -t lisp\" \"define\\\\* \\\\(exec\")
"
  (define f (format #f "~a [cli-command]" m))
  (when verbose
    (format #t "~a utility    : ~a\n" f utility)
    (format #t "~a gx-dry-run : ~a\n" f gx-dry-run)
    (format #t "~a params     : ~a\n" f params)
    (format #t "~a args       : ~a\n" f args))
  (let* [(elements (list #:verbose #:utility #:gx-dry-run #:params))
         (args (remove-all-elements args elements))]
    (if gx-dry-run
        (begin
          ;; <stdin>:1494:40: warning: possibly unbound variable `monad'
          ;; (format #t "~a monad: ~a\n" f monad)
          (format #t "~a TODO implement --gx-dry-run\n" f))
        ((comp
          ;; (lambda (p) (format #t "~a done\n" f) p)
          exec-full-command
          ;; (lambda (p) (format #t "~a 2. ~a\n" f p) p)
          (partial append (list params))
          ;; (lambda (p) (format #t "~a 1. ~a\n" f p) p)
          (partial map (partial format #f "~s"))
          ;; (lambda (p) (format #t "~a 0. ~a\n" f p) p)
          )
         args))))

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
