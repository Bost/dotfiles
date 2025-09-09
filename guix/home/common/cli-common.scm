(define-module (cli-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (utils)
  #:export (exec-full-command cli-command))

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
(cli-command                 #:params \"ls -la\" \"rest\" \"args\")"
  (define f (format #f "~a [git-command]" m))
  (when verbose
    (format #t "~a utility    : ~a\n" f utility)
    (format #t "~a gx-dry-run : ~a\n" f gx-dry-run)
    (format #t "~a params     : ~a\n" f params)
    (format #t "~a args       : ~a\n" f args))
  (let* [(elements (list #:verbose #:utility #:gx-dry-run #:params))
         (args (remove-all-elements args elements))]
    (if gx-dry-run
        (begin
          (format #t "~a monad: ~a\n" f monad)
          (format #t "~a TODO implement --gx-dry-run\n" f))
        ((comp
          ;; (lambda (p) (format #t "~a done\n" f) p)
          exec-full-command
          ;; (lambda (p) (format #t "~a 0. ~a\n" f p) p)
          )
         (append (list params) args)))))

(module-evaluated)
