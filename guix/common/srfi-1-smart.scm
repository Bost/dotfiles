(define-module (srfi-1-smart)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)  ; define*-public
 )

(define*-public (smart-first obj)
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

(define*-public (smart-last obj)
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

(define*-public (smart-second obj)
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

(define*-public (smart-third obj)
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

(define*-public (smart-fourth obj)
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

(define*-public (smart-fifth obj)
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

(define*-public (smart-take obj n)
  "Takes first n elements from list or first n characters from string."
  (cond
    ((string? obj)
     (substring obj 0 (min n (string-length obj))))
    ((list? obj)
     (take obj n))
    (else
     (error "not a list or string" obj))))

(define*-public (smart-drop obj n)
  "Drops first n elements from list or first n characters from string."
  (cond
    ((string? obj)
     (let ((len (string-length obj)))
       (substring obj (min n len) len)))
    ((list? obj)
     (drop obj n))
    (else
     (error "not a list or string" obj))))

(define*-public (smart-length obj)
  "TODO Consider moving smart-length to a different module. It has no equivalent
in (srfi srfi-1)."
  (cond
   ((string? obj)
    (string-length obj))
   ((list? obj)
    (length obj))
   ((pair? obj)
    2)
   (else
    (error "not a list or string or pair" obj))))

(define*-public (smart-append . args)
"(apply smart-append (list \"1\" \"2\" \"3\"))           ; => \"123\"
(apply smart-append (list (list 1 2) (list 3 4))) ; => (1 2 3 4)
(apply symbol-append (list 'a 'b))                ; => ab

(apply smart-append (list (cons 1 2) (cons 3 4))) ; => exception

TODO Consider moving smart-append to a different module. It has no equivalent
in (srfi srfi-1)."
  (cond
   ((list? (car args))
    (apply append args))
   ((string? (car args))
    (apply string-append args))
   ((symbol? (car args))
    (apply symbol-append args))
   (else
    (error "First object not a list or string" args))))

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
