(define-module (scm-bin git-command)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main git-command))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ command) -s
!#

cd $dotf
./guix/home/common/scm-bin/command.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-command command #:rest args)
  "Usage:
(git-command \"fetch\" \"-f\" \"arg0\")
(git-command \"fetch\" \"-f arg0\")
(equal? (git \"fetch\" \"-f\" \"arg0\")
        (git \"fetch\" \"-f arg0\"))
;; > #t
"
  (define f (format #f "~a [git-command]" m))

  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (p) (format #t "~a done\n" f) p)
    (lambda* (full-command)
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
    ;; (lambda (p) (format #t "~a 0. ~a\n" f p) p)
    )
   (append (list "git" command) args)))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply git-command)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
