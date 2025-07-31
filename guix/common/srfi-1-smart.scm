(define-module (srfi-1-smart)
  #:use-module (srfi srfi-1)
  #:export (
            smart-first
            smart-last
            smart-second
            smart-third
            smart-fourth
            smart-fifth
            smart-take
            smart-drop
            ))

(define (smart-first obj)
  "Returns first element of list or first character of string as a string."
  (cond
    ((string? obj)
     (if (string=? obj "")
         (error "empty string")
         (string (string-ref obj 0))))
    ((pair? obj)
     (car obj))
    ((null? obj)
     (error "empty list"))
    (else
     (error "not a list or string" obj))))

(define (smart-last obj)
  "Returns last element of list or last character of string as a string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (if (= len 0)
           (error "empty string")
           (string (string-ref obj (- len 1))))))
    ((pair? obj)
     (last obj))
    ((null? obj)
     (error "empty list"))
    (else
     (error "not a list or string" obj))))

(define (smart-second obj)
  "Returns second element of list or second character of string as a string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (if (< len 2)
           (error "string too short")
           (string (string-ref obj 1)))))
    ((pair? obj)
     (let ((rest (cdr obj)))
       (if (null? rest)
           (error "list too short")
           (car rest))))
    ((null? obj)
     (error "empty list"))
    (else
     (error "not a list or string" obj))))

(define (smart-third obj)
  "Returns third element of list or third character of string as a string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (if (< len 3)
           (error "string too short")
           (string (string-ref obj 2)))))
    ((pair? obj)
     (let ((rest (cddr obj)))
       (if (null? rest)
           (error "list too short")
           (car rest))))
    ((null? obj)
     (error "empty list"))
    (else
     (error "not a list or string" obj))))

(define (smart-fourth obj)
  "Returns fourth element of list or fourth character of string as a string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (if (< len 4)
           (error "string too short")
           (string (string-ref obj 3)))))
    ((pair? obj)
     (fourth obj))
    ((null? obj)
     (error "empty list"))
    (else
     (error "not a list or string" obj))))

(define (smart-fifth obj)
  "Returns fifth element of list or fifth character of string as a string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (if (< len 5)
           (error "string too short")
           (string (string-ref obj 4)))))
    ((pair? obj)
     (fifth obj))
    ((null? obj)
     (error "empty list"))
    (else
     (error "not a list or string" obj))))

(define (smart-take obj n)
  "Takes first n elements from list or first n characters from string."
  (cond
    ((string? obj)
     (substring obj 0 (min n (string-length obj))))
    ((list? obj)
     (take obj n))
    (else
     (error "not a list or string" obj))))

(define (smart-drop obj n)
  "Drops first n elements from list or first n characters from string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (substring obj (min n len) len)))
    ((list? obj)
     (drop obj n))
    (else
     (error "not a list or string" obj))))

#|
;; Example usage:
(display "Testing smart functions:\n")

;; Test with lists
(display (smart-first '(a b c d)))      ; => a
(newline)
(display (smart-last '(a b c d)))       ; => d
(newline)
(display (smart-second '(a b c d)))     ; => b
(newline)

;; Test with strings
(display (smart-first "hello"))         ; => "h"
(newline)
(display (smart-last "hello"))          ; => "o"
(newline)
(display (smart-second "hello"))        ; => "e"
(newline)
(display (smart-third "hello"))         ; => "l"
(newline)

;; Test smart-take and smart-drop
(display (smart-take "hello" 3))        ; => "hel"
(newline)
(display (smart-drop "hello" 2))        ; => "llo"
(newline)
(display (smart-take '(1 2 3 4 5) 3))   ; => (1 2 3)
(newline)
(display (smart-drop '(1 2 3 4 5) 2))   ; => (3 4 5)
(newline)
|#
