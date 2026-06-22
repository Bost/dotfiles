#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-system-describe) -s
!#

(define-module (scm-bin guix-system-describe)
  #:use-module (scm-bin describe-commits)
  #:use-module (dotf utils)
  #:use-module (srfi srfi-1)     ; list-processing procedures
  #:use-module (srfi srfi-13)    ; string library
  #:use-module (ice-9 optargs)   ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-system-describe) -s
!#

cd $dotf
./guix/home/common/scm-bin/guix-system-describe.scm | tee /dev/tty | xsel -bi

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (hex-char? c)
  "Is C a hexadecimal digit?"
  (or (and (char>=? c #\0) (char<=? c #\9))
      (and (char>=? c #\a) (char<=? c #\f))
      (and (char>=? c #\A) (char<=? c #\F))))

(define (commit-token? s)
  "Does S look like a git commit, i.e. exactly 40 hex characters?"
  (and (= (string-length s) 40) (string-every hex-char? s)))

(define (parse-channels lines)
  "lines of `guix system describe' -> ((NAME . COMMIT) ...), NAME a symbol.
A channel header is a bare `name:' line; its commit is the 40-hex token on a
following line.  Keys on shapes rather than field labels, so it survives Guix's
localized labels (commit:/branche:/URL du depot:, etc.)."
  (reverse
   (cdr
    (fold
     (lambda (line state)
       (let* ((current (car state))
              (acc     (cdr state))
              (t       (string-trim-both line))
              (toks    (string-tokenize line)))
         (cond
          ((string-suffix? ":" t)
           (cons (string->symbol (string-trim-right (string-drop-right t 1))) acc))
          ((and (pair? toks) (commit-token? (last toks)))
           (cons current (cons (cons current (last toks)) acc)))
          (else state))))
     (cons #f '())
     lines))))

(define* (parse-generation-date #:key args)
  "lines -> date string from the first (generation) line, taken verbatim.
Drops the localized `Generation' word, the number, and the trailing `(current)'
marker, so the date reads in whatever locale the command ran under."
  ((comp
    (lambda (toks) (string-join toks " "))
    (lambda (toks) (filter (lambda (s) (not (string-prefix? "(" s))) toks))
    cddr
    string-tokenize
    (lambda (lines)
      (or (find (lambda (l) (not (string-null? (string-trim-both l)))) lines) "")))
   args))

(define*-public (main #:rest args)
  "Usage:
(main \"<ignored>\")
CLI args are ignored; prints the #:NAME-commit block for the running system's
channels, as reported by `guix system describe'."
  ((comp
    print-lines
    (lambda (lines) (commit-block (parse-generation-date #:args lines)
                                  (parse-channels lines)))
    ;; peek
    (lambda (argv) (run-command #:args argv))
    (lambda (_) '("guix" "system" "describe"))   ; ignore CLI args
    ;; peek
    )
   args))

(module-evaluated)
