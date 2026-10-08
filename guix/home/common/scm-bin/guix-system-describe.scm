(define-module (scm-bin guix-system-describe)
  #:use-module (scm-bin describe-commits)
  #:use-module (bost common utils)
  #:use-module (srfi srfi-1)     ; list-processing procedures
  #:use-module (srfi srfi-13)    ; string library
  #:use-module (ice-9 optargs)   ; define*
  )

#|

#!/usr/bin/env -S guix repl -L ./guix/home/common --
!#

cd $dotf
# not '(apply main (command-line))'
echo -e "\n(main (command-line))" >> ./guix/home/common/scm-bin/guix-system-describe.scm
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
  "lines -> date string from the first (generation) line. E.g.
\"Generation 42  Oct 08 2026 13:58:57  (current)\" -> \"8 October 2026 13:58\"
Drops the `Generation' word, the number, and the trailing `(current)' marker.
The lines must come from the C locale. See `format-date'."
  ((comp
    format-date
    car
    (lambda (s) (setlocale LC_TIME "C") (strptime "%b %d %Y %H:%M:%S" s))
    (lambda (toks) (string-join toks " "))
    (lambda (toks) (filter (lambda (s) (not (string-prefix? "(" s))) toks))
    cddr
    string-tokenize
    (lambda (lines)
      (or (find (lambda (l) (not (string-null? (string-trim-both l)))) lines) "")))
   args))

(define-public (main args)
  "Usage:
(main (list \"<ignored>\"))
CLI args are ignored; prints the #:NAME-commit block for the running system's
channels, as reported by `guix system describe'."
  ((comp
    print-lines
    (lambda (lines) (commit-block (parse-generation-date #:args lines)
                                  (parse-channels lines)))
    ;; peek
    (lambda (argv) (run-command #:args argv))
    ;; ignore CLI args; LC_ALL=C for English and unlocalized labels
    (lambda (_) '("env" "LC_ALL=C" "guix" "system" "describe"))
    cdr
    ;; peek
    )
   args))

(module-evaluated)
