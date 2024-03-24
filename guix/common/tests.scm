(define-module (tests)
  #:use-module (utils)
  ;; #:use-module (ice-9 rdelim)
  ;; #:use-module (ice-9 popen)
  ;; #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1) ;; fold remove
  #:use-module (ice-9 exceptions) ;; guard
  #:use-module (system syntax internal) ;; syntax?
  #:use-module (guix gexp) ;; contains extended reader for #~ #$ #+ #$@
  #:use-module (guix build utils) ;; find-files
  #| #:use-module (language cps intmap) |#)

(define-syntax do-test
  (syntax-rules ()
    ((_ symbol arg ...)
     ((comp
       (lambda (function)
         (when (guard (ex (else #f)) (function arg ...))
           symbol))
       #;(lambda (p) (format #t "p: ~a\n" p) p))
      (eval symbol (interaction-environment))))))

(define-public (true? x) (eq? x #t))
(define-public (false? x) (eq? x #f))

(define-public (test-type o)
  "Type Testing Predicates.
(test-type (call-with-input-string \"  (+ x y)\" read-syntax)) ; => (syntax?)
(test-type '())     ; => (list? null?)
(test-type \"a\")   ; => (string?)
(test-type 1)       ; => (complex? real? integer? number?)
(test-type (+ 1 2)) ; => (complex? real? integer? number?)
(test-type (* 3-8i 2.3+0.3i)) ; => (complex? number?)
(test-type #\\space)           ; => (char-whitespace?)
(test-type #\\a)               ; => (char-alphabetic?)
(test-type #\\1)               ; => (char-numeric?)
(test-type (gexp 42))          ; => (gexp?)
"
  ((comp
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol o))))
   (list
    'unspecified?
    'boolean?
    'true?
    'false?
    'port?
    'string?
    'symbol?
    'list?
    'vector?
    'procedure?
    'complex?
    'real?
    'integer?
    'number?
    'syntax?
    'gexp?
    'pair?
    'char?
    'null?
    'parameter? ;; ? is this for macros ?
    'zero?
    'positive?
    'negative?
    'odd?
    'even?
    'eof-object?
    'char-alphabetic?
    'char-numeric?
    'char-whitespace?
    )))

(define (test-equality a b)
  "Equality and Comparison Predicates.
(test-equality 1 2)       ; => ()
(test-equality 1 1)       ; => (eq? eqv? equal?)
(test-equality \"1\" \"1\")   ; => (eq? eqv? equal?)
"
  ((comp
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol a b))))
   (list
    'string=? ;; returns #t only if both parameters are strings
    'eq?
    'eqv?
    'equal?)))

(define-public (syntax->list orig-ls)
  "From $der/racket/pkgs/racket-benchmarks/tests/racket/benchmarks/common/psyntax-input.txt

(syntax->list (call-with-input-string \"  (+ 1 2)\" read-syntax))
"
  (let f ((ls orig-ls))
    (syntax-case ls ()
      (() '())
      ((x . r) (cons (syntax x) (f (syntax r))))
      (_ (error 'syntax->list "invalid argument ~s" orig-ls)))))

