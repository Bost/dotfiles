(define-module (cli-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (utils)
  #:export (
            cli-general-command

            cli-command
            cli-background-command
            cli-system-command
            ))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (cli-common) -s
!#

cd $dotf
./guix/home/common/cli-common.scm 'define\* \(exec'
|#


(define m (module-name-for-logging))
(evaluating-module)

(define* (cli-general-command    #:key (verbose #f) gx-dry-run params fun exec-fun #:allow-other-keys #:rest args)
  "The ARGS are being ignored.
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
  (define f (format #f "~a [cli-general-command]" m))
  (when verbose
    (format #t "~a gx-dry-run : ~s\n" f gx-dry-run)
    (format #t "~a params     : ~s\n" f params)
    (format #t "~a fun        : ~s\n" f fun)
    (format #t "~a exec-fun   : ~s\n" f exec-fun)
    (format #t "~a args       : ~s\n" f args))
  (let* [(elements (list #:verbose #:gx-dry-run #:params #:fun #:exec-fun))
         (args (remove-all-elements args elements))]
    (if gx-dry-run
        (begin
          ;; <stdin>:1494:40: warning: possibly unbound variable `monad'
          ;; (format #t "~a monad: ~a\n" f monad)
          (format #t "~a TODO implement --gx-dry-run\n" f))
        (begin
          ((comp
            ;; (lambda (p) (format #t "~a done\n" f) p)
            ;; (partial apply (partial exec-fun params))
            exec-fun
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

(define* (cli-command            #:key (verbose #f) utility gx-dry-run params #:allow-other-keys #:rest args)
  (define f (format #f "~a [cli-command]" m))
  ((comp
    (partial apply cli-general-command)
    ;; (lambda (v) (format #t "~a 1 ~a\n" f v) v)
    (partial append (list #:exec-fun exec-foreground))
    ;; (lambda (v) (format #t "~a 0 ~a\n" f v) v)
    )
   args))

(define* (cli-background-command #:key (verbose #f) utility gx-dry-run params exec-fun #:allow-other-keys #:rest args)
  ((comp
    (partial apply cli-general-command)
    (partial append (list #:exec-fun exec-background)))
   args))

(define* (cli-system-command     #:key (verbose #f) utility gx-dry-run params #:allow-other-keys #:rest args)
  ((comp
    (partial apply cli-general-command)
    (partial append (list #:exec-fun (partial apply system))))
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
