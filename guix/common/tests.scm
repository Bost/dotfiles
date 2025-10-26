(define-module (tests)
  #:use-module (utils)
  ;; #:use-module (ice-9 rdelim)
  ;; #:use-module (ice-9 popen)
  ;; #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1) ;; fold remove

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
         ;; need to specify (ice-9 exceptions) for `guard' to avoid warnings
         ;; because of `error?' being defined in both (ice-9 exceptions) and
         ;; (rnrs conditions)
         (when ((@(ice-9 exceptions) guard)
                (condition [else #f])
                ;; (format #t "function        : ~a\n" function)
                ;; (format #t "symbol          : ~a\n" symbol)
                ;; (format #t "arg ...         : ~a\n" arg ...)
                ;; (format #t "(list? arg ...) : ~a\n" (list? arg ...))
                (if (list? arg ...)
                    (apply function arg ...)
                    (function arg ...)))
           (cond
            [(and (list? symbol)
                  ;; error? can be from (ice-9 exceptions) or (rnrs conditions)
                  (not (equal? 'error? (last symbol))))
             (caddr symbol)]
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
(test-type 1)       ; => (number? complex? real? integer? rational? positive? odd?)
(test-type (+ 1 2)) ; => (number? complex? real? integer? rational? positive? odd?)
(test-type (* 3-8i 2.3+0.3i)) ; => (complex? number?)
(test-type #\\space)           ; => (char-whitespace?)
(test-type #\\a)               ; => (char-alphabetic?)
(test-type #\\1)               ; => (char-numeric?)
(test-type (gexp 42))          ; => (gexp?)
(test-type (make-error))       ; => (record? exception? error?)
(test-type (make-exception))   ; => (record? exception?)
(test-type (/ 0.0 0.0))        ; => (number? complex? real? nan?)
(test-type (sqrt -1.0))        ; => (number? complex?)

(nan? (sqrt -1.0)) ; => Wrong type argument in position 1: 0.0+1.0i

(test-type (make-exception ((@(ice-9 exceptions) make-error))))
; => (record? exception? (@ (ice-9 exceptions) error?) condition?)

(test-type (make-exception ((@(rnrs conditions) make-error))))
; => (record? exception? (@ (rnrs conditions) error?) (@ (ice-9 exceptions) error?) condition?)
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
    ;;
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
    ;; NaN - symbol to indicate that a mathematical operation could not produce
    ;; a meaningful result
    'nan?
    ;;
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
    ;;
    '(@(gnu services) service?)
    '(@(gnu services) service-type?)
    '(@(gnu services) service-extension?)
    ;;
    'exception?
    '(@(rnrs conditions) error?)
    '(@(ice-9 exceptions) error?)

    ;; Conditions are records of a subtype of the &condition record type, which
    ;; is neither sealed nor opaque. See R6RS Records.
    '(@(rnrs conditions) condition?)
    '(@(rnrs conditions) violation?)
    )))

(define-public (test-equality . args)
  "Equality and Comparison Predicates. Variadic (i.e. infinite arity)
(test-equality 1 2)       ; => ()
(test-equality 1 1)       ; => (= eq? eqv? equal?)
(test-equality \"1\" \"1\")   ; => (string=? string-ci=? eq? eqv? equal?)
(test-equality 1 1 1)     ; => (= eq? eqv? equal?)
"
  ((comp
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test symbol args))))
   (list
    'string=? ;; returns #t only if both parameters are strings
    'string-ci=?
    'char=?
    'char-ci=?

    'eq?
    'eqv?
    'equal?)))
