(define-module (tests)
  #:use-module (utils)
  ;; #:use-module (ice-9 rdelim)
  ;; #:use-module (ice-9 popen)
  ;; #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1) ;; fold remove
  #:use-module (ice-9 exceptions) ;; guard

  ;; The syntax? and gexp? may not be defined when resolved by '#:use-module'.
  ;; Use module scoping '@@' instead.
  ;; #:use-module (system syntax internal) ;; syntax?
  ;; #:use-module (guix gexp) ;; gexp?, and extended reader for #~ #$ #+ #$@

  #:use-module (guix build utils) ;; find-files
  #| #:use-module (language cps intmap) |#)

(define-syntax do-test
  (syntax-rules ()
    ((_ symbol arg ...)
     ((comp
       (lambda (function)
         (when (guard (ex (else #f)) (function arg ...))
           (cond
            [(list? symbol) (caddr symbol)]
            [#t symbol])
           ))
       #;(lambda (p) (format #t "p: ~a\n" p) p))
      (eval symbol (interaction-environment))))))

;;; ### BEG: from ~/dev/guile/module/ice-9/boot-9.scm
(define (valid-import? x)
  (list? x))

(define (valid-export? x)
  (or (symbol? x) (and (pair? x) (symbol? (car x)) (symbol? (cdr x)))))

(define (valid-autoload? x)
  (and (pair? x) (list-of symbol? (car x)) (list-of symbol? (cdr x))))

;;; ### END: from ~/dev/guile/module/ice-9/boot-9.scm

(define-public (test-type o)
  "See predicates https://en.wikipedia.org/wiki/Scheme_(programming_language)

Type Testing Predicates.
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
    '(@(utils) true?)
    '(@(utils) false?)
    'port?
    'string?
    'symbol?
    'list?
    'vector?
    'procedure?
    'record?

    'number?
    'complex?
    'real?
    'integer?
    'rational?
    'positive?
    'negative?
    'odd?
    'even?
    'zero?

    '(@(system syntax internal) syntax?)
    'identifier?   ;; #t if syntax-object is an identifier, or #f otherwise.
    '(@(guix gexp) gexp?)
    'pair?
    'char?
    'null?
    'parameter? ;; ? is this for macros ?
    'eof-object?
    'char-alphabetic?
    'char-numeric?
    'char-whitespace?

    '(@(gnu services) service?)
    '(@(gnu services) service-type?)
    '(@(gnu services) service-extension?)
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
    'string-ci=?
    'char=?
    'char-ci=?

    'eq?
    'eqv?
    'equal?)))

