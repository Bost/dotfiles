#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-describe) -s
!#

(define-module (scm-bin guix-describe)
  #:use-module (scm-bin describe-commits) ; commit-block, run-command, print-lines
  #:use-module (dotf utils)               ; comp, partial, logging helpers
  #:use-module (srfi srfi-1)              ; fold-right
  #:use-module (ice-9 optargs)            ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-describe) -s
!#

cd $dotf
./guix/home/common/scm-bin/guix-describe.scm | tee /dev/tty | xsel -bi

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (current-generation-date)
  "-> \"18 juin 2026 15:35:17\"
Generation date of ~/.config/guix/current, from its ctime -- the same thing
`guix describe` stats -- so it stays correct regardless of which profile the
running Guile belongs to.  Localized via the environment (LC_ALL > LC_TIME > LANG)."
  (setlocale LC_TIME "")
  (strftime "%e %B %Y %H:%M:%S"
            (localtime
             (stat:ctime
              (stat (string-append (getenv "HOME") "/.config/guix/current"))))))

(define (unquote-symbol x)
  "(quote SYM) -> SYM; SYM -> SYM."
  (if (and (pair? x) (eq? (car x) 'quote)) (cadr x) x))

(define (channel-form->pair form)
  "(channel (name 'N) ... (commit \"C\") ...) -> (N . \"C\")."
  (let ((clauses (cdr form)))
    (cons (unquote-symbol (cadr (assq 'name clauses)))
          (cadr (assq 'commit clauses)))))

(define* (describe->pairs #:key args)
  "lines of `guix describe --format=channels` -> ((NAME . COMMIT) ...).
Reads the (list (channel ...) ...) form and pulls name/commit straight out of
each channel clause -- a pure reader, no eval and no (guix channels) import, so
this stays a leaf module whose whole closure is pure Scheme."
  ((comp
    (lambda (forms) (map channel-form->pair forms))
    cdr                                            ; drop the leading `list'
    (lambda (text)  (call-with-input-string text read))
    (lambda (lines) (fold-right (lambda (s acc) (string-append s "\n" acc)) "" lines)))
   args))

(define*-public (main #:rest args)
  "Usage:
(main \"<ignored>\")
CLI args are ignored; prints the #:NAME-commit block for the channels reported
by `guix describe --format=channels`."
  ((comp
    print-lines
    (lambda (pairs) (commit-block (current-generation-date) pairs))
    ;; peek
    (lambda (lines) (describe->pairs #:args lines))
    (lambda (argv)  (run-command #:args argv))
    (lambda (_) '("guix" "describe" "--format=channels"))   ; ignore CLI args
    ;; peek
    )
   args))

(module-evaluated)
