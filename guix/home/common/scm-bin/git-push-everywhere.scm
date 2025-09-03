(define-module (scm-bin git-push-everywhere)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (scm-bin git-remote)
  #:use-module (scm-bin git-command)
  #:export (main git-push-everywhere))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-push-everywhere) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-push-everywhere.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-push-everywhere #:rest args)
  "Push commits to all remote repositories."
  (define f (format #f "~a [git-push-everywhere]" m))

  ;; (format #t "~a Starting…\n" f)
  (call/cc
   (lambda (exit)
     ((comp
       ;; (lambda (p) (format #t "~a done\n" f) p)
       ;; (lambda (p) (format #t "~a 3. ~a\n" f p) p)
       (lambda (ret-vals) ;; the reducer
         ;; ret-vals is a list consisting of sublists:
         ;; ((ret-code-0 list-of-vals-0)
         ;;  (ret-code-N list-of-vals-N))
         ;; where ret-code-i is a number, list-of-vals-i can consist of any values
         (list (apply max (map car ret-vals)) (map cdr ret-vals)))

       ;; (lambda (x) (format #t "~a Exiting\n" f) (exit x))
       ;; (lambda (p) (format #t "~a 2. ~a\n" f p) p)
       (partial map
                (lambda (remote)
                  (apply (partial git-command "push"
                                  "--follow-tags"
                                  "--verbose"
                                  remote)
                         args)))
       ;; (lambda (p) (format #t "~a 1. ~a\n" f p) p)
       cdr ;; ignore the return code
       ;; (lambda (p) (format #t "~a 0. ~a\n" f p) p)
       )
      (git-remote))))
  )
(testsymb 'git-push-everywhere)

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"arg0\")"
  ((comp
    (partial apply git-push-everywhere)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
