(define-module (tests)
  #:use-module (utils)
  ;; #:use-module (ice-9 rdelim)
  ;; #:use-module (ice-9 popen)
  ;; #:use-module (ice-9 regex)
  ;; #:use-module (srfi srfi-1) ;; fold
  #:use-module (system syntax internal) ;; syntax?
  #| #:use-module (language cps intmap) |#)

(define-syntax do-test
  (syntax-rules ()
    ((_ symbol arg ...)
     ((compose
       (lambda (function) (when (function arg ...) symbol))
       #;(lambda (p) (format #t "p: ~a\n" p) p))
      (eval symbol (interaction-environment))))))

(define (test-type o)
  "Type Testing Predicates.
(test-type (call-with-input-string \"  (+ x y)\" read-syntax)) ; => (syntax?)
(test-type '())     ; => (list? null?)
(test-type \"a\")   ; => (string?)
(test-type 1)       ; => (complex? real? integer? number?)
(test-type (+ 1 2)) ; => (complex? real? integer? number?)
(test-type (* 3-8i 2.3+0.3i)) ; => (complex? number?)
"

  ((compose
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol o))))
   (list
    'boolean?
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
    'pair?
    'char?
    'null?
    'parameter? ;; ? is this for macros ?
    )))

(define (test-equality a b)
  "Equality and Comparison Predicates.
(test-equality 1 2)       ; => ()
(test-equality 1 1)       ; => (eq? eqv? equal?)
(test-equality \"1\" \"1\")   ; => (eq? eqv? equal?)
"
  ((compose
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol a b))))
   (list
    'eq?
    'eqv?
    'equal?
    ;; works only if both parameters are strings
    ;; 'string=?
    )))

(define (test-type-numeric o)
  "Numeric Predicates.
Doesn't work for complex numbers. TODO continue the tests only if (number? o) succeeds
(test-type-numeric (* 3-8i 2.3+0.3i)) ; TODO
"
  ((compose
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol o))))
   (list
    'zero?
    'positive?
    'negative?
    'odd?
    'even?
    )))

(define (test-type-other o)
  "Other Useful Predicates.
(test-type-other (* 3-8i 2.3+0.3i)) ; TODO
(test-type-other #\\space) ; => (char-whitespace?)
"
  ((compose
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol o))))
   (list
    'eof-object?
    'char-alphabetic?
    'char-numeric?
    'char-whitespace?)))
