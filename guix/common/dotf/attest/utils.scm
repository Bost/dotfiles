(define-module (dotf attest utils)
  #:use-module (srfi srfi-1)   ; list-processing procedures
  #:use-module (srfi srfi-19)  ; string->date
  #:use-module (srfi srfi-88)  ; provides keyword objects
  #:export
  (
   clamp
   days-between
   logistic
   parse-tstp
   unique-plist-get
   ))

(read-set! keywords 'prefix) ; Allow both :keyword and #:keyword

(define (get-keys lst)
  "Return a list of all keys in the list LST, which may or may not be a plist.
It stops at last complete pair.

(get-keys '(:a 1 b 2))  ; => (:a b)
(get-keys '(a 1 b 2))   ; => (a b)
(get-keys '())          ; => ()
(get-keys '(:a 1 :a 3)) ; => (:a :a) ; not checking for duplicate keys
(get-keys '(:a 1 :b))   ; => (:a)    ; :b is silently dropped
(get-keys 1)            ; => not a list
"
  (unless (list? lst)
    (error "get-keys: not a list" lst))

  (let loop ((xs lst) (acc '()))
    (cond
     ((or (null? xs) (null? (cdr xs)))
      (reverse acc))
     (else
      (loop (cddr xs) (cons (car xs) acc))))))

(define (has-duplicates? lst)
  "Used in `unique-plist?'
(has-duplicates? '())        ; => #f
(has-duplicates? '(1 2 3 4)) ; => #f
(has-duplicates? '(1 2 3 2)) ; => #t
(has-duplicates? '(a 1 a 2)) ; => #t
"
  (cond
   ((null? lst) #f)
   ((member (car lst) (cdr lst)) #t)
   (else (has-duplicates? (cdr lst)))))

(define (unique-plist? lst)
  "Empty list is also a plist. Plist must not contain duplicate keys.

TODO Git-commit-like metadata may repeat keys (trailers, parents, etc.); in that
case use a different representation or a non-unique plist predicate. When
addressing this, use `plist?' instead of this procedure in the `unique-plist-get'.

It has complexity O(n) from `length' + O(n^2) from `member'.
TODO Reimplement using hash-table/set-based when running at scale.

(unique-plist? '(a 1 b 2)) ; => #t
(unique-plist? '())        ; => #t
(unique-plist? '(1))       ; => #f ; odd number of elements
(unique-plist? '(1 2 3))   ; => #f ; odd number of elements
(unique-plist? 1)          ; => #f ; not a list
(unique-plist? '(a 1 a 2)) ; => #f ; 'a is a duplicate key
"
  (and (list? lst) (even? (length lst))
       (not (has-duplicates? (get-keys lst)))))

(define (plist? lst)
  "Shape check only: proper list of even length.

(plist? '(a 1 b 2)) ; => #t
(plist? '())        ; => #t
(plist? '(1))       ; => #f ; odd number of elements
(plist? '(1 2 3))   ; => #f ; odd number of elements
(plist? 1)          ; => #f ; not a list
(plist? '(a 1 a 2)) ; => #t
"
  (and (list? lst) (even? (length lst))))

(define (unique-plist-get . args)
  "Works with arguments in either order.
TODO error-out when a dangling key is encountered. E.g. :b in in '(:a 1 :b)

(unique-plist-get '(:y 2 :x 1) :x)         ; => 1
(unique-plist-get :x (list :y 2 :x 1))     ; => 1
(unique-plist-get '(:x 1 :x 2) :x)         ; unique key/value pairs
(unique-plist-get '(:y 2 :x 1) :z)         ; => #f
(unique-plist-get '() :x)                  ; => #f
(unique-plist-get '(1 11 2 22) 1)          ; => 11
(unique-plist-get '((1 2) 11 2 22) '(1 2)) ; => 11
(unique-plist-get '(42 :y 2 :x 1) :x)      ; length is not even
(unique-plist-get)                         ; expected exactly 2 arguments
(unique-plist-get 1)                       ; expected exactly 2 arguments
(unique-plist-get '())                     ; expected exactly 2 arguments
"
  (define (loop lst key)
    (cond [(null? lst) #f]
          [(eq? (car lst) key) (cadr lst)]
          [else (loop (cddr lst) key)]))

  (unless (= 2 (length args))
    (error
     "unique-plist-get: expected exactly 2 arguments (lst key) or (key lst)"
     args))

  (let* ((loop-args (if (list? (car args))
                        args
                        (reverse args)))
         (lst (car loop-args)))
    (if (unique-plist? lst)
        (apply loop loop-args)
        (let ((explanation
               ;; Just copy-paste the and-clauses from the unique-plist? into
               ;; '(not )'. Don't overthink it.
               (cond
                ((not (list? lst))
                 "not a list")
                ((not (even? (length lst)))
                 "list length is not even")
                ;; invariant: predicate here is exactly (not C) where C is the
                ;; last conjunct
                ((not (not (has-duplicates? (get-keys lst))))
                 "list must contain unique keys")
                (else "invalid unique-plist"))))
          (error (string-append "unique-plist-get: " explanation) lst)))))

(define (parse-tstp tstp-string)
  "Parse \"YYYY-MM-DD_HH-MM-SS\" into an SRFI-19 date.
Example:
(parse-tstp \"2026-01-12_19-07-32\") ; =>
#<date nanosecond: 0 second: 32 minute: 7 hour: 19 day: 12 ... >
"
  (string->date tstp-string "~Y-~m-~d_~H-~M-~S"))

(define (days-between begin end)
  "Return number of whole days between BEGIN and END (both SRFI-19 dates).
Examples:
(days-between (parse-tstp \"2026-01-01_00-00-00\")
              (parse-tstp \"2026-01-10_00-00-00\")) ; => 9
"
  (let* ((beg-time     (date->time-utc begin))
         (end-time     (date->time-utc end))
         (delta-time (- (time-second end-time) (time-second beg-time))))
    ;; (* 24 #| hours |# 60 #| minutes |# 60 #| seconds |#) ; => 86400
    ;; (let ((delta-time 172799)) ;; 1 day, 23 hours, 59 minutes, 59 seconds
    ;;   (format #t
    ;;           "(/ delta-time 86400.0)    : ~a\n(inexact->exact ...) : ~a\n"
    ;;           (/ delta-time 86400.0)
    ;;           (inexact->exact (floor (/ delta-time 86400)))))
    (inexact->exact (floor (/ delta-time 86400)))))

(define (clamp x lo hi)
  "Force X to stay between LO and HI. (Clamp - wood working tool.)
Examples:
(clamp 1 2 3) => 2
(clamp 5 1 9) => 5
"
  (max lo (min hi x)))

(define (logistic x)
  "Sigmoid / logistic curve https://en.wikipedia.org/wiki/Logistic_function
It maps any real number to a number strictly between 0 and 1:
- Very negative x → result close to 0
- x = 0 → result exactly 0.5
- Very positive x → result close to 1
It turns an unbounded score into something that behaves like a probability."
  (/ 1.0 (+ 1.0 (exp (- x)))))

