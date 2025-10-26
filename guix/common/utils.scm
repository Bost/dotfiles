;;; This module is required by some of the scm-bin CLI utilities. The output of
;;; the `format' will also appear in the console such a utility is executed.

;; TODO create a package installable by `guix install my=utils`
;; See: jaro the resource opener - an alternative to xdg-open
;; https://github.com/isamert/jaro/blob/master/jaro
;; See `guile-build-system'

;; TODO rename (utils) to (dotf utils)
(define-module (utils)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   1. service-file -> with-imported-modules
;;;   2. common-modules
  #:use-module (srfi-1-smart)
  #:use-module (ice-9 match)  ; match
  #:use-module (ice-9 popen)  ; open-input-pipe
;;; (ice-9 readline) requires `guix install guile-readline'.
  ;; #:use-module (ice-9 readline)
  #:use-module (ice-9 rdelim) ; read-line
  #:use-module (ice-9 regex)  ; regexp and string matching
  #:use-module (srfi srfi-1)  ; list-processing procedures
  ;; #:use-module (guix build utils) ;; invoke - not needed
  #:use-module (ice-9 pretty-print)

  ;; WTF? following line leads to 'no code for module (guix read-print)'
  ;; #:use-module (guix read-print)

  #:use-module (ice-9 string-fun) ; string-replace-substring
  #:use-module (guix monads)      ; return, bind
  #:use-module (rnrs io ports)    ; exec-with-error-to-string

  ;; for inferior-package-in-guix-channel : beg
  ;; #:use-module (guix channels)  ;; WTF? not found by `guix home ...`
  ;; #:use-module (guix inferior)
  ;; #:use-module (guix packages)
  ;; #:use-module (guix profiles) ;; probably not needed
  ;; for inferior-package-in-guix-channel : end
  #:use-module (ice-9 exceptions) ; guard

  #:export (
            compose-commands-guix-shell
            compose-commands-guix-shell-dry-run
            compose-shell-commands
            compute-cmd
            contains--gx-dry-run?
            dbg-exec
            dbg-packages-to-install
            def*
            empty?
            error-command-failed
            evaluating-module
            exec
            exec-background
            exec-foreground
            exec-system*
            exec-system
            exec-system*-new
            exec-with-error-to-string
            if-let
            if-not
            module-evaluated
            testsymb
            testsymb-trace
            dbgfmt
            sha1-file
            str-join
            timestamp
            pretty-print-with-comments->string
            map-indexed
            )
  #:re-export (
               smart-first
               smart-last
               smart-second
               smart-third
               smart-fourth
               smart-fifth
               smart-take
               smart-drop
               ))

(define m "[utils]")
;; (format #t "~a evaluating module…\n" m)

;; https://github.com/daviwil/dotfiles/tree/master/.config/guix
;; Also (examples)
;; (use-service-modules nix)
;; (use-service-modules desktop xorg)
;; (use-package-modules certs)
;; (use-package-modules shells)

;;;;;; beg: testsymb, testsymb-trace

;; neither `=' nor `eqv?' work
(define-public cnt length+)

(define-public (partial fun . args)
  "Alternative implementation:
(use-modules (srfi srfi-26))
(map (cut * 2 <>) '(1 2 3 4)) ;; => (2 4 6 8)"
  (lambda x (apply fun (append args x))))

(define-public (comp . fns)
  "Like `compose'. Can be called with zero arguments. I.e. (thunk? comp) => #t
Works also for functions returning and accepting multiple values."
  (lambda args
    (if (null? fns)
        (apply values args)
        (let [(proc (car fns)) (rest (cdr fns))]
          (if (null? rest)
              (apply proc args)
              (let ((g (apply comp rest)))
                (call-with-values (lambda () (apply g args)) proc)))))))

(define-public (juxt . fns)
  "Naive implementation. Inspired by Clojure's juxt.
((juxt a b c) x) => (list (a x) (b x) (c x))

;; Example usage:
(define add1 (lambda (x) (+ x 1)))
(define square (lambda (x) (* x x)))
(define negate (lambda (x) (- x)))

;; Create a juxtaposition of functions
(define combined (juxt add1 square negate))

;; Apply to arguments
(combined 5)  ; => (6 25 -5)

;; With multiple arguments
(define add (lambda (x y) (+ x y)))
(define mult (lambda (x y) (* x y)))
(define sub (lambda (x y) (- x y)))

(define math-ops (juxt add mult sub))
(math-ops 10 3)  ; => (13 30 7)

;; Using with built-in functions
(define string-ops (juxt string-length string-upcase string-downcase))
(string-ops \"Hello\")  ; => (5 \"HELLO\" \"hello\")"
  (lambda args
    (map (lambda (fn) (apply fn args)) fns)))

(define eq-op? string-ci=?)
(define-public (s+ . rest) (apply (partial lset-union eq-op?) rest))
(define-public (s- . rest) (apply (partial lset-difference eq-op?) rest))
(define-public (sx . rest) (apply (partial lset-intersection eq-op?) rest))

(define-public empty? null?) ;; no runtime cost. null? is a primitive procedure

(define-public (boolean x) (not (not x)))

(define-public (str . args)
  "Convert all arguments to strings and concatenate them, like Clojure's `str`."
  (string-concatenate
   (map (lambda (x)
          (cond
           ((string? x) x)
           ((symbol? x) (symbol->string x))
           ((number? x) (number->string x))
           ((char? x) (string x))
           ((boolean? x) (if x "#t" "#f"))
           ((empty? x) "()")
           ;; (use-modules (ice-9 format))  ; For `format` with ~A specifier
           ((pair? x) (format #f "~A" x))   ; Handle lists and pairs
           (else (format #f "~A" x))))      ; Fallback for other types
        args)))

;; (warn ...) doesn't print anything
(define-public (my=warn . args)
  ;; (error s)
  (let* [(orig-fmt (car args))
         (fmt (if (string= "\n" (smart-last orig-fmt))
                  orig-fmt
                  (str orig-fmt "\n")))]
    (apply (partial format #t (str "W " fmt))
           (cdr args))))

(define-public (module-name-for-logging)
  ((comp
    (partial format #f "[~a]")
    (partial string-join)
    (partial map (partial format #f "~a"))
    (partial module-name))
   (current-module)))

(unless (equal? (module-name-for-logging) m)
  (my=warn "~a (equal? (module-name-for-logging) m): ~a"
           m (equal? (module-name-for-logging) m)))

(define-syntax if-let
  (syntax-rules ()
    [(_ (var test) then)
     (let [(var test)]
       (if var then))]
    ;; same as the above just adding the brackets
    [(_ ((var test)) then)
     (let [(var test)]
       (if var then))]

    [(_ (var test) then else)
     (let [(var test)]
       (if var then else))]
    ;; same as the above just adding the brackets
    [(_ ((var test)) then else)
     (let [(var test)]
       (if var then else))]))
#|
(if-let (result (+ 2 2))
        (format #t "Truthy Test Passed: ~a\n" (number->string result))
        (format #t "Truthy Test Failed: Should not reach here\n"))

(if-let (result (and #f (+ 2 2)))
        (format #t "Falsey Test Failed: Should not reach here\n")
        (format #t "Falsey Test Passed: Correctly reached else clause\n"))
|#

(define show-evaluating-module #f) ; placed at the beginning of a module
(define show-module-evaluated  #f) ; placed at the end of a module

(define (inf-evaluating-module)
  (format #t "~a Evaluating module…\n" (module-name-for-logging)))

(define-syntax evaluating-module
  (syntax-rules ()
    [(_ show) (when show                   (inf-evaluating-module))]
    [(_)      (when show-evaluating-module (inf-evaluating-module))]))

(define (inf-evaluating-module-done)
  (format #t "~a Evaluating module… done\n" (module-name-for-logging)))

(define-syntax module-evaluated
  (syntax-rules ()
    [(_ show) (when show                  (inf-evaluating-module-done))]
    [(_)      (when show-module-evaluated (inf-evaluating-module-done))]))

(define (warn-undefined symbol)
  (my=warn (format #f "~a Symbol undefined: ~a"
                   (module-name-for-logging) symbol)))

(define-syntax testsymb
  (syntax-rules ()
    [(_ symbol)
     (unless (defined? symbol) (warn-undefined symbol))
     ;; (if (defined? symbol)
     ;;     (format #t "~a Symbol defined: ~a\n" (module-name-for-logging) symbol)
     ;;     (warn-undefined symbol))
     ]))

(define-syntax testsymb-trace
  (syntax-rules ()
    [(_ symbol)
     (if (defined? symbol)
         (format #t "~a Symbol defined: ~a\n" (module-name-for-logging) symbol)
         (warn-undefined symbol))]))

(define (test-testsymb)
  (define f 42)
  (testsymb 'f)
  (testsymb-trace 'f)
  )

;;;;;; end: testsymb, testsymb-trace

(define-public (true? x) (eq? x #t))

(define-public (false? x) (eq? x #f))

;;; testsymb doesn't work in the let-syntax
;; (let [(ff 42)] (testsymb 'ff))

(define-public (pretty-print->string sexp)
  (let [(port (open-output-string))]
    (pretty-print sexp port)
    (let* [(ret (get-output-string port))]
      (close-output-port port)
      ret)))

(define* (pretty-print-with-comments->string sexp #:key (max-width 78))
  (call-with-output-string
    (lambda (port)
      ;; can't use '#:use-module (guix read-print)'. See above module definition
      ((@(guix read-print) pretty-print-with-comments) port sexp
       #:max-width max-width))))

(define-public (unspecified-or-empty-or-false? obj)
  (or (unspecified? obj)
      (null? obj)
      (and (string? obj) (string-null? obj))
      (eq? #f obj)))

;; TODO turn `path' to a procedure named `env-path' or `pathenv'
;; Turn the colon-separated PATH-string, into a list and return the resulting
;; list with tail appended
(define-public path
  (delete-duplicates (parse-path (getenv "PATH"))))

(define-public (has-suffix? string suffix)
  "Does STRING end with the SUFFIX? As `string-suffix?' but the parameters are
reversed. See also:
(define s (string-match \"[0-9][0-9][0-9][0-9]\" \"blah2002foo\"))
(match:end s) ;; ⇒ 8"
  (string-suffix? suffix string))

(define-public ends-with? has-suffix?)

(define-public (has-substring? str subs)
  (boolean (string-match subs str)))

(define-public (drop-right xs n)
  "(drop-right (list 1 2 3 4 5) 2) ;; => (1 2 3)
Corresponds to `drop-last' in Clojure"
  (reverse (list-tail (reverse xs) n)))

(define-public (drop-left xs n)
  "(drop-left (list 1 2 3 4 5) 2) ;; => (4 5)
Corresponds to `drop' in Clojure"
  (reverse (list-head (reverse xs) n)))

(define-public (flatten x)
  "(flatten (list (cons 1 (cons 2 3))))
   ;; => (1 2 3)
   (equal? (list 1 2 3)
           (flatten (list (cons 1 (cons 2 3)))))
   ;; => #t
"
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define (fmt-rest rest)
  (if (empty? rest)
      ""
      (format #f "~a" (string-join (map str rest)))))

;; TODO dbgfmt should be smart to detect if the symbols `f' and/or `m' are defined and if so then use them
(define-syntax dbgfmt
  ;; match specific datums `m' and `f' in an expression
  (syntax-rules (m f)
    [(_ m f e ...)
     (format #t "~a ~a ~a\n" m f (fmt-rest (list e ...)))]
    [(_ f m e ...) ;; in case we have reversed order: `f m'
     (format #t "~a ~a ~a\n" m f (fmt-rest (list e ...)))]
    [(_ f e ...)
     (format #t "~a ~a\n" f (fmt-rest (list e ...)))]
    [(_ m e ...)
     (format #t "~a ~a\n" m (fmt-rest (list e ...)))]
    [(_ e ...)
     (format #f "~a\n" (fmt-rest (list e ...)))]))

;; TODO implement pretty-print for bash commands
(define-public (dbg prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "~s\n" prm)
  prm)

(define* (dbg-exec prm #:key (verbose #t))
  "`pk', i.e. `peek' can be used instead of this function"
  (when verbose
    (format #t "§ ~a\n" (if (list? prm) (string-join prm) prm)))
  prm)

(define* (error-command-failed #:rest args)
  "Returns #t and prints \"Command failed.\" with some extra text. Does NOT
error-out!"
  (define f "[error-command-failed]")
  ;; (format #t "~a ~a Starting ...\n" f m)
  (define (error-fun . args)
    ;; (error (apply (partial format #f (car args))
    ;;               (cdr args)))
    (apply (partial format (current-error-port))
           (cons (str "E " (car args) "\n") (cdr args)))
    )
  (match args
    ['()
     (error-fun "Command failed.")]
    [(module)
     (error-fun "~a Command failed." module)]
    [(module extra-text)
     (error-fun "~a Command failed: ~a" module extra-text)]))

(define-public (split-string s n)
  "Split a string S into substrings of length N.
Examples:
;; Valid cases
(split-string \"hello\" 2)          ; => (\"he\" \"ll\" \"o\")
(split-string \"hello\" 5)          ; => (\"hello\")
(split-string \"hello\" 10)         ; => (\"hello\") - n >= length
(split-string \"\" 3)               ; => (\"\") or maybe should be '()?
(split-string \"ab\" 1)             ; => (\"a\" \"b\")
(split-string \"hello\" 0)          ; => (\"hello\")

;; Invalid n: not a number
(split-string \"hello\" \"2\")        ; => error
(split-string \"hello\" #f)         ; => error
(split-string \"hello\" '())        ; => error
(split-string \"hello\" #\\c)        ; => error

;; Invalid n: negative number
(split-string \"hello\" -1)         ; => error
(split-string \"hello\" -5)         ; => error

;; Invalid n: non-integer number
(split-string \"hello\" 2.5)        ; => error
(split-string \"hello\" 1.0)        ; => error (even though mathematically = 1)
(split-string \"hello\" +inf.0)     ; => error
(split-string \"hello\" +nan.0)     ; => error

;; Invalid s: not a string
(split-string 123 2)              ; => error
(split-string #f 2)               ; => error
(split-string '() 2)              ; => error
(split-string #\\h 2)              ; => error

;; Both invalid
(split-string 123 \"2\")            ; => error
(split-string #f -1)              ; => error

;; Edge cases
(split-string \"\" 0)               ; => (\"\")
(split-string \"a\" 1)              ; => (\"a\")
"
  (define (error-out s n)
    (define f "[split-string]")
    (let* [(s1 (if (not (and (number? n)
                             (or (zero? n) (positive? n))
                             (integer? n)))
                   (format #f "`n' '~a' must be a positive integer or zero" n)
                   ""))
           (s2 (if (not (string? s))
                   (format #f "`s' must be a string") ""))
           (s ((comp
                (partial str-join ", ")
                (partial remove unspecified-or-empty-or-false?))
               (list s1 s2)))]
      (error (format #f "~a ~a" f s))))

  (if  (and (string? s) (number? n))
       (cond
        [(and (positive? n) (integer? n))
         (if (<= (string-length s) n)
             (list s)
             (cons (substring s 0 n)
                   (split-string (substring s n) n)))]
        [(zero? n) (list s)]
        [else (error-out s n)])
       (error-out s n)))

(define-public (smart-split-string s n)
  "Smart split a string S into substrings of length N.
Example:
(smart-split-string \"12345\" 2) ; => (\"12\" \"34\" \"5\")
(smart-split-string 2 \"12345\") ; => (\"12\" \"34\" \"5\")
"
  (if (and (string? s) (number? n))
      (split-string s n)
      (split-string n s)))

(define (split-space-escaped input)
  "(split-space-escaped \"a b\\ c\") ;; => (\"a\" \"b c\")"
  (let* [(placeholder "#\\space")]
    ((comp
      (partial map (lambda (s) (string-replace-substring s placeholder " ")))
      (lambda (prepared) (string-split prepared #\space))
      (lambda (input) (string-replace-substring input "\\ " placeholder)))
     input)))

(define (string-sff ch s-list)
  "(string-sff #\\space (list \"foo bar baz\"))
;; => (\"foo\" \"bar\" \"baz\")"
  ((comp
    (partial filter (comp not string-null?))
    flatten
    (partial map (lambda (s) (string-split s ch))))
   s-list))

(define-public (ensure-list x)
  "Wrap X in a list if it is not already a list. Think \"monadic container\"."
  (match x
    ((? list?) x)
    (else (list x))))

;; ;; TODO consider testing for proper-list?
;; ;; See also (scm-error 'wrong-type-arg #f "Expected a proper list or non-pair value" #f #f)
;; (define-public (ensure-list x)
;;   "Wrap X in a list if it is not already a list. Think \"monadic container\".
;; Raise an error if it's neither a list nor a single value."
;;   (guard (exception ((not (or (list? exception)
;;                               (not (pair? exception))))
;;                      => (lambda (x) (error "Expected a list or single value" x))))
;;     ;; Alternative implementations:
;;     ;;   (or (and (list? x) x)
;;     ;;       (list x))
;;     ;; or
;;     ;;   (match args
;;     ;;     ((_ ...) args)
;;     ;;     (x (list x)))
;;     (match x
;;       ((? list?) x)
;;       (else (list x)))))

(define-public (string-split-whitespace one-or-more-args)
  ((comp
    ;; (partial string-sff #\space)
    flatten (partial map split-space-escaped)
    (partial string-sff #\newline)
    (partial string-sff #\tab)
    ensure-list)
   one-or-more-args))

(define dry-run-prm "--gx-dry-run")

(define* (contains--gx-dry-run? args)
  (or (and (list? args) (member dry-run-prm args))
      (and (string? args) (string-contains args dry-run-prm))))

(define* (exec-or-dry-run exec-function args)
  (if (contains--gx-dry-run? args)
      args
      (if (list? args)
          (apply exec-function args)
          (exec-function args))))

(define* (exec-system* #:key (verbose #t) #:rest args)
  "Execute system command and returns its ret-code. E.g.:
(exec-system* \"echo\" \"bar\" \"baz\") ;; =>
$ (echo bar baz)
bar baz
$9 = 0 ;; return code"
  (let* [(f "[exec-system*]")
         (elements (list #:verbose))
         (args (remove-all-elements args elements))]
    ;; (format #t "~a ~a args : ~a\n" m f args)
    ((comp
      (partial exec-or-dry-run system*)
      (lambda (prm) (dbg-exec prm #:verbose verbose))
      ;; TODO fix exec-system*: string-split-whitespace also splits:
      ;;   "(@(bost gnu packages emacs-xyz) ~a)"
      string-split-whitespace)
     args)))

(define* (exec-or-dry-run-new #:key exec-function (gx-dry-run #f) (verbose #f) #:rest args)
  (define f (format #f "~a [exec-or-dry-run-new]" m))
  (let* [(elements (list #:exec-function #:gx-dry-run #:verbose))
         (args (remove-all-elements args elements))

         (args (car args))]
    ;; (format #t "~a ~a exec-function : ~a\n" m f exec-function)
    ;; (format #t "~a ~a dry-run       : ~a\n" m f
    ;;         (or (contains--gx-dry-run? args) gx-dry-run))
    ;; (format #t "~a ~a args          : ~a\n" m f args)
    (if (or (contains--gx-dry-run? args) gx-dry-run)
        0 ;; the exit status OK
        (if (list? args)
            (apply exec-function args) ;; TODO add #:verbose
            (exec-function args)))))

(define* (exec-system*-new
          #:key (split-whitespace #t) (gx-dry-run #f) (verbose #t)
          #:rest args)
  "Execute system command and returns its ret-code. E.g.:
(exec-system* \"echo\" \"bar\" \"baz\") ;; =>
$ (echo bar baz)
bar baz
$9 = 0 ;; return code"
  (define f (format #f "~a [exec-system*-new]" m))
  (let* [(elements (list #:split-whitespace #:gx-dry-run #:verbose))
         (args (remove-all-elements args elements))]
    ;; (format #t "~a ~a split-whitespace : ~a\n" m f split-whitespace)
    ;; (format #t "~a ~a gx-dry-run       : ~a\n" m f gx-dry-run)
    ;; (format #t "~a ~a args             : ~a\n" m f args)
    ;; (format #t "~a ~a (list? args)     : ~a\n" m f (list? args))
    ;; (format #t "~a ~a (length args)    : ~a\n" m f (length args))
    ((comp
      (lambda (exit-status)
        ;; (format #t "~a ~a exit-status       : ~a\n" m f exit-status)
        ;; (format #t "~a ~a (= exit-status 0) : ~a\n" m f (= exit-status 0))
        (exit (= exit-status 0)))
      (partial exec-or-dry-run-new
               #:gx-dry-run gx-dry-run
               #:verbose verbose
               #:exec-function system*)
      (lambda (prm) (dbg-exec prm #:verbose verbose))
      (partial map (lambda (s) (if split-whitespace (string-split-whitespace s) s))))
     args)))

(define-public (read-all reader-function)
  "Returns a function which reads all lines of text from the PORT and applies
READER-FUNCTION on them. "
  (lambda (port)
    (let loop [(res '())
               (str (reader-function port))] ; from (ice-9 popen)
      (if (and str (not (eof-object? str)))
          (loop (append res (list str))
                (reader-function port))
          res))))

(define-public (read-all-sexprs p)
  "TODO better implementation of read-all-sexprs"
  (let loop [(x (read p))]
    (if (eof-object? x)
        '()
        (cons x (loop (read p))))))

(define-public (read-all-syntax port)
  "Return a list of all s-expressions from the PORT."
  ((read-all read-syntax) port))

(define-public (read-all-strings port)
  "Return a list of all lines, i.e. a list of string of text from the PORT."
  ((read-all read-line) port))

(define-public (cmd->string cmd)
  (if (list? cmd)
      ;; join with ' ' by default
      (string-join
       (remove unspecified-or-empty-or-false?
               cmd))
      cmd))

;; 8sync https://www.gnu.org/software/8sync/
;; asynchronous programming library for GNU Guile. Based on the actor
;; model, it makes use of delimited continuations to avoid a mess of callbacks
;; resulting in clean, easy to read non-blocking code.
#;(import (language wisp spec)) ;; whitespace lisp

(define* (exec-background command #:key (verbose #f))
  "Execute COMMAND in background, i.e. in a detached process.
COMMAND can be a string or a list of strings.
§ echo bar baz & disown
bar baz
$9 = 0 ;; <return-code>"
  (define f (format #f "~a [exec-background]" m))
  ((comp
    (partial exec-or-dry-run system)
    ;; (lambda (prm) (dbg-exec prm #:verbose verbose))
    (lambda (prm) (dbg-exec prm #:verbose #t))
    cmd->string
    ;; disown belongs to shells. See `help disown`. The semicolon, as indicated
    ;; by `help disown` ivoked from the fish-shell, in eg. `echo foo &; disown`,
    ;; doesn't work in bash, only in the fish-shell
    (lambda (cmd) (list cmd "&" "disown"))
    cmd->string)
   command))

(define* (exec-foreground command #:key (verbose #f))
  "Execute COMMAND and returns its ret-code.
E.g.:
(exec-foreground \"echo bar baz\") ;; =>
§ echo bar baz
bar baz
$9 = (0 \"bar baz\") ;; (<return-code> <return-value>)"
  (define f (format #f "~a [exec-foreground]" m))
  (let* [(cmd-result-struct (exec command #:return-plist #t))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        (begin
          (map (partial format #t "~a\n")
               (plist-get cmd-result-struct #:results))
          cmd-result-struct)
        (begin
          (error (format #f "~a retcode: ~a\n" f retcode)) ; error-out
          ;; (error-command-failed f)
          ;; or return `retcode' instead of `*unspecified*'
          ;; *unspecified*
          ))))

(define* (exec-system command #:key (verbose #f))
  "Execute COMMAND using `system' from the (guile) module and returns its ret-code.
E.g.:
(exec-system \"echo bar baz\") ;; =>
§ echo bar baz
bar baz
$9 = 0 ;; <return-code>"
  (define f (format #f "~a [exec-system]" m))
  ((comp
    (partial exec-or-dry-run system)
    ;; (lambda (prm) (dbg-exec prm #:verbose verbose))
    (lambda (prm) (dbg-exec prm #:verbose #t)))
   command))

;; (define (background-system command)
;;   " https://sourceware.org/legacy-ml/guile/1998-09/msg00228.html "
;;   (let ((child-pid (primitive-fork)))
;;     (if (zero? child-pid)
;;         ;; Okay, we're the child process.  We need to catch any and
;;         ;; all errors and exit, or else we'll end up with two Guile
;;         ;; repls trying to read from the same terminal.
;;         (begin
;;           (catch #t
;;             (lambda ()
;;               ;; Put ourselves in our own process group.
;;               (setpgid (getpid) (getpid))
;;               ;; Try to execute the user's command.
;;               (execl "/bin/sh" "sh" "-c" command))
;;             (lambda args #f))
;;           ;; If we return from the exec for any reason, it means it failed.
;;           (quit 1))
;;         ;; Okay, we're the parent process.  Return the child pid, in
;;         ;; case we want to wait for it at some point in the future.
;;         child-pid)))

;;; See https://www.draketo.de/software/guile-capture-stdout-stderr.html
;; (format #t "current-error-port:\n~a\n"
;;         (let* ((error-port (open-output-string)))
;;           (with-error-to-port error-port
;;             (lambda ()
;;               (display "Err msg1\n" (current-error-port))))
;;           (get-output-string error-port)))

;; (format #t "(output-port? error-port): ~a\n"
;;         (let* ((error-port (open-output-string)))
;;           (output-port? error-port)))

(define* (exec-with-error-to-string commad #:key (verbose #t))
  "Run the shell COMMAND using '/bin/sh -c' with 'OPEN_READ' mode, ie. to read
from the subprocess. Wait for the command to terminate and return 3 values:
- `#t' if the port was successfully closed or `#f' if it was already closed.
- a string containing standard output
- a string containing standard error output

Usage:
(use-module (ice-9 receive)) ;; or (srfi srfi-8)
(receive (retval stdout stderr)
    (exec-with-error-to-string \"echo to-stdout; echo to-stderr >&2\")
  (format #t \"receive retval:~a\\n\" retval)
  (format #t \"receive stdout:~a\\n\" stdout)
  (format #t \"receive stderr:~a\\n\" stderr))
"
  (define (exec-function commad)
    (if (string-contains commad dry-run-prm)
        (values "" "" "")
        (let* ((err-cons (pipe))
               (port (with-error-to-port (cdr err-cons)
                       (lambda () (open-input-pipe commad))))
               ;; the err-cons buffer size is 16 MiB
               (_ (setvbuf (car err-cons) 'block (* 1024 1024 16)))
               (stdout (read-delimited "" port)))
          (values
           (close-port (cdr err-cons))
           stdout
           ;; the port must be closed before calling the following
           (read-delimited "" (car err-cons))))))

  ((comp
    ;; Can't use the (partial exec-or-dry-run exec-function) since the
    ;; exec-function returns multiple values and the exec-or-dry-run is able to
    ;; return only one value.
    exec-function
    (lambda (prm) (dbg-exec prm #:verbose verbose))
    cmd->string)
   commad))

(define* (exec command #:key (verbose #t) (return-plist #f))
  "Run the shell COMMAND using '/bin/sh -c' with 'OPEN_READ' mode, ie. to read
from the subprocess. Wait for the command to terminate and return a string
containing its output.

RETURN-PLIST - return property list which can be accessed by:
(plist-get (exec \"echo 'foo'\" #:return-plist #t) #:retcode)

TODO have a look if a delimited continuation can be used to break out of `exec',
i.e. skip the `read-all-strings' and thus make `exec-background' out of it.

Usage:
(define (process retcode output)
  (format #t \"(test-type output): ~a\\n\" (test-type output))
  ...
  retcode)

(let* [(cmd-result-struct (exec \"echo foo\" #:return-plist #t))
       (retcode (plist-get cmd-result-struct #:retcode))]
  (if (zero? retcode)
      (process retcode (plist-get cmd-result-struct #:results))
      (begin
        ;; (error (format #f \"~a retcode: ~a\n\" f retcode)) ; error-out
        ;; (error-command-failed f \"extra_info\")
        ;; or return `retcode' instead of `*unspecified*'
        *unspecified*)))

Or:

(let* [(ret (exec (list \"echo\" \"foo\")))
       (retcode (car ret))]
    (if (= 0 retcode)
        (let* ((output (cdr ret)))
          (process retcode output))
      (begin
        ;; (error (format #f \"~a retcode: ~a\n\" f retcode)) ; error-out
        ;; (error-command-failed f \"extra_info\")
        ;; or return `retcode' instead of `*unspecified*'
        *unspecified*)))
"
  ;; ,use (guix build utils) ;; contains `invoke'
  ;; `invoke' does `(apply system* program args)'; `system*' waits for the
  ;; program to finish, The command is executed using fork and execlp.

  ;; TODO write a scheme procedure: chdir str
  ;; There may be a chdir procedure in guix source code somewhere.

  ;; Change the current working directory to str. The return value is
  ;; unspecified.
  (define (exec-function command)
    ;; Can't use the `call-with-port' since the exit-val is needed.
    (let* [(port (open-input-pipe command)) ; from (ice-9 rdelim)
           ;; the `read-all-strings' must be called before `close-pipe'.
           (results (read-all-strings port))]
      (if return-plist
          (list
           #:retcode (status:exit-val (close-pipe port))
           #:results results)
          (cons
           (status:exit-val (close-pipe port))
           results))))
  ((comp
    (partial exec-or-dry-run exec-function)
    (lambda (prm) (dbg-exec prm #:verbose verbose))
    cmd->string)
   command))

(define-public (analyze-pids-flag-variable user init-cmd client-cmd pids)
  "Breakout implementation using a flag variable"
  (let [(ret-cmd init-cmd)]
    ((comp
      (lambda (p)
        (if (null? p)
            init-cmd ;; No such binary has been started yet.
            (car p)))
      (partial
       map
       (lambda (pid)
         (when (string=? ret-cmd init-cmd) ;; check the flag
           (let ((proc-user ((comp
                              cadr
                              exec)
;;; '-o' user defined format, 'h' no header, '-p' pid
                             (format #f "ps -o user= h -p ~a" pid))))
             (when (and (not (string-null? proc-user))
                        (string=? user proc-user))
               (let ((proc-cmd (exec
;;; '-o' user defined format, 'h' no header, '-p' pid
                                (format #f "ps -o command= h -p ~a" pid))))
                 (unless (string-match ".*<defunct>$" proc-cmd)
;;; Set a flag so that the body of the outermost when-statement is not executed
                   (set! ret-cmd client-cmd))))))
         ret-cmd)))
     pids)))

(define-public (analyze-pids-call/cc user init-cmd client-cmd pids)
  "For a process ID from the list of PIDS, return the INIT-CMD if no process ID was
found or the CLIENT-CMD if some process ID was found."
  (call/cc
   (lambda (continuation)
     (map
      (lambda (pid)
        (let ((proc-user ((comp
                           cadr
                           exec)
;;; '-o' user defined format, 'h' no header, '-p' pid
                          (format #f "ps -o user= h -p ~a" pid))))
          (when (and (not (string-null? proc-user))
                     (string=? user proc-user))
            (let ((proc-cmd ((comp cadr exec)
;;; '-o' user defined format, 'h' no header, '-p' pid
                             (format #f "ps -o command= h -p ~a" pid))))
              (unless (string-match ".*<defunct>$" proc-cmd)
;;; Terminate the call/cc statement with the return value `client-cmd'
                (continuation client-cmd))))))
      pids)
     ;; The pids-list is empty. No such binary has been started yet.
     init-cmd)))

(define* (compute-cmd #:key user init-cmd client-cmd pgrep-pattern)
  "pgrep for a USER and PATTERN and return the INIT-CMD if no process ID was found
or the CLIENT-CMD if some process ID was found."
  ((comp
    (partial
     analyze-pids-call/cc
     ;; analyze-pids-flag-variable
     user init-cmd client-cmd)
    cdr
    exec
    ;; --euid effective ID
    (partial format #f "pgrep --full --euid ~a ~a" user)
    (lambda (s)
      ;; TODO either:
      ;; (A) remove single quotes, escape double quotes, or
      ;; (B) escape backslashes and spaces
      (str "\"" s "\"")))
   pgrep-pattern))

;; Like `define*' but it prints what's being defined / evaluated
;; See /home/bost/dev/guile/module/ice-9/psyntax.scm line 3377
;; Introduces unhygienic `f'!!!
(define-syntax def*
  (lambda (x)
    (syntax-case x ()
      [(_ (identifier . args) b0)
       (with-syntax ((f (datum->syntax #'identifier 'f)))
        #'(begin
            ;; (format #t "[fa] (def* (~a…)…)…\n" `identifier)
            (define identifier
              (let [(f (format #f "~a [~a]" m `identifier))]
                (cond
                 [#t                    ;; fa
                  (lambda* args
                    ;; (format #t "~a Starting…\n" f)
                    (let [(result b0)]
                      ;; (format #t "~a done\n" f)
                      result))])))
            ;; (format #t "[fa] (def* ~a…)… done\n" `identifier)
            identifier))]

      [(_ (identifier . args) b0 b1)
       (with-syntax ((f (datum->syntax #'identifier 'f)))
        #'(begin
            ;; (format #t "[fb fc] (def* (~a…)…)…\n" `identifier)
            (define identifier
              (let [(f (format #f "~a [~a]" m `identifier))]
               (cond
                [(string? `b0)         ;; fb
                 (lambda* args
                   b0
                   ;; (format #t "~a Starting…\n" f)
                   (let [(result b1)]
                     ;; (format #t "~a done\n" f)
                     result))]

                [#t                    ;; fc
                 (lambda* args
                   ;; (format #t "~a Starting…\n" f)
                   b0
                   (let [(result b1)]
                     ;; (format #t "~a done\n" f)
                     result))])))
            ;; (format #t "[fb fc] (def* ~a…)… done\n" `identifier)
            identifier))]

      [(_ (identifier . args) b0 b1 ... bN)
       (with-syntax ((f (datum->syntax #'identifier 'f)))
        #'(begin
            ;; (format #t "[fd fe] (def* (~a…)…)…\n" `identifier)
            (define identifier
              (let [(f (format #f "~a [~a]" m `identifier))]
               (cond
                [(string? `b0)         ;; fd
                 (lambda* args
                   b0
                   ;; (format #t "~a Starting…\n" f)
                   b1 ...
                   (let [(result bN)]
                     ;; (format #t "~a done\n" f)
                     result))]

                [#t                    ;; fe
                 (lambda* args
                   ;; (format #t "~a Starting…\n" f)
                   b0
                   b1 ...
                   (let [(result bN)]
                     ;; (format #t "~a done\n" f)
                     result))])))
            ;; (format #t "[fd fe] (def* ~a…)… done\n" `identifier)
            identifier))]

      [(_ identifier val) (identifier? #'identifier) ;; ff
       #'(begin
           ;; (format #t "[ff] (def* ~a…)…\n" `identifier)
           (define identifier val)
           ;; (format #t "[ff] (def* ~a…)… done\n" `identifier)
           identifier)])))

;; ;;; Test cases:
;; (def* (fa a b)
;;   "fa: some output string")

;; (def* (fb a b)
;;   "fb: docstring"
;;   42)

;; (def* (fc a b)
;;   (format #t "fc: output 1\n")
;;   (format #t "fc: output 2\n"))

;; (def* (fd a b)
;;   "fd: docstring"
;;   (format #t "output 1\n")
;;   (format #t "output 2\n"))

;; (def* (fe a b)
;;   (format #t "output 1\n")
;;   (format #t "output 2\n")
;;   (format #t "output 3\n"))

;; (def* (fe a b)
;;   "fe: docstring"
;;   (format #t "~a a ~a\n" f a)
;;   (format #t "~a b ~a\n" f b)
;;   42)

;; (def* ff 42)

;; Like `define-public' but it prints what's being defines
(define-syntax def-public
  (syntax-rules ()
    ((_ (name . args) . body)
     (begin
       (format #t "(def-public (~a…)…)… " `name)
       (define (name . args) . body)
       (format #t "done\n")
       (export name)))
    ((_ name val)
     (begin
       (define name val)
       (format #t "done\n")
       (export name)))))

;; from /home/bost/dev/guile/module/ice-9/boot-9.scm
;; (define-syntax define-public
;;   (syntax-rules ()
;;     ((_ (name . args) . body)
;;      (begin
;;        (define (name . args) . body)
;;        (export name)))
;;     ((_ name val)
;;      (begin
;;        (define name val)
;;        (export name)))))

(define-public (mktmpfile)
  "Create / Make temporary file under /tmp"
  ;; (tmpnam) could be used instead of all of this, however I get deprecation
  ;; warning sometimes
  ((comp
    port-filename
    mkstemp!
    ;; prevent the 'string is read-only ...' error
    string-copy)
   "/tmp/myfile-XXXXXX"))

;; TODO add install-recursively to (guix build utils) and send it to upstream
(define* (install-recursively source destination
                              #:key
                              (log (current-output-port))
                              (follow-symlinks? #f)
                              (copy-file copy-file)
                              keep-mtime? keep-permissions?)
  "Recursive version of install-file."
  ((@(guix build utils) mkdir-p) destination)
  ((@(guix build utils) copy-recursively)
   source
   (string-append destination "/" (basename destination))
   #:log log
   #:follow-symlinks? follow-symlinks?
   #:copy-file copy-file
   #:keep-mtime? keep-mtime?
   #:keep-permissions? keep-permissions?
   ))

(define-public (url? url)
  "Is URL a valid url?"
  (or (string-prefix? "https://" url)
      (string-prefix? "http://" url))
  #;
  (let ((url-regex (rx (and string-start
                            (or "http" "https" "ftp") "://"
                            (ensure-list (not (any " ")))
                            string-end))))
    (regexp-match url-regex url)))

(define (plist-get-original plist key)
  "Original plist-get implementation."
  (cond
   ((null? plist) #f)
   ((equal? (car plist) key) (cadr plist))
   (else (plist-get-original (cddr plist) key))))

(define-public (plist-get . args)
  "Smart plist-get that works with arguments in either order.
Examples:
(plist-get (list :y 2 #:x 1) #:x)   ; => 1
(plist-get #:x (list :y 2 #:x 1))   ; => 1
(plist-get (list :y 2 #:x 1) #:z)   ; => #f"
  ((comp
    (lambda (params) (apply plist-get-original params))
    (lambda (args)
      (if (list? (car args))
          args
          (reverse args))))
   args))

(define (remove-element lst element)
  "Remove all occurrences of a given element from a list.
If the element is a keyword (e.g., #:x), it also removes the next element (its
value).

(remove-element '(x #:a 1 x #:b 2 #:c 3 x b z #:d 1 #:d) #:d)
; => (x #:a 1 x #:b 2 #:c 3 x b z)"
  (define (recur xs)
    (cond
     ((null? xs) '())
     ((and (keyword? element)      ; If element is a keyword and matches...
           (eq? (car xs) element))
      (if (null? (cdr xs))
          '() ; no value after keyword; remove only keyword
          (recur (cddr xs))))   ; ...then skip the keyword and its value

     ((equal? (car xs) element) ; If element matches...
      (recur (cdr xs)))         ; ...then skip it

     (else
      (cons (car xs) (recur (cdr xs))))))

  (recur lst))

(define-public (remove-all-elements lst elements)
  "Remove all elements from a list.
If an element is a keyword (e.g., #:x), also remove the following element (its
value).

Example:
(remove-all-elements '(x #:a 1 y #:b 2 #:c 3 x z #:d 1 #:d) '(x #:d))
=> (#:a 1 y #:b 2 #:c 3 z)"
  (let ((uniq-elements (delete-duplicates elements)))
    (let loop ((lst lst)
               (els uniq-elements))
      (if (null? els)
          lst
          (loop (remove-element lst (car els))
                (cdr els))))))

(define-inlinable (pipe-return params)
  (list
   ;; Return code signaling that some hypothetical previous command terminated
   ;; successfully.
   0
   ;; String containing the parameters of the next command
   params))

(define-inlinable (pipe-bind mv mf)
  (let* [(mv-retcode (plist-get mv #:retcode))]
    (if (zero? mv-retcode)
        (mf (plist-get mv #:results))
        (begin
          (error-command-failed m (format #f "mv-retcode: ~a" mv-retcode))
          mv))))

;; See guix/common/utils.scm
(define-monad compose-shell-commands
  (bind pipe-bind)
  (return pipe-return))

(define-inlinable (guix-shell-return lst-params)
  (list
   ;; Return code signaling that some hypothetical previous command terminated
   ;; successfully.
   ;; copy-file, delete-file return *unspecified*
   lst-params  ;; output returned by some previous command
   ;; String containing the parameters of the next command
   lst-params))

(define-inlinable (guix-shell-bind mv mf)
  "Contains (mf lst-params)"
  ;; (format #t "\n")
  ;; (format #t "mv: ~a\n" mv)
  ;; (format #t "mf: ~a; (list? mf): ~a\n" mf (list? mf))
  (if (and (list? mf) (equal? (car mf) 'override-mv))
      (begin
        ;; (format #t "mf: (cdr mf): ~a\n" (cdr mf))
        (guix-shell-return (cdr mf)))
      (let* [(lst-output-of-previous-cmd (car mv))
             (lst-params (cadr mv))]
        (if (equal? lst-output-of-previous-cmd lst-params)
            (begin
              (format #t "~s… " `(,mf ,lst-params))
              ;; (format #t "~s…\n" `(,mf ,lst-params))
              (mf lst-params)
              ;; (format #t "~s… done\n" `(,mf ,lst-params))
              (format #t "done\n")
              ;; enforce manual command params specification by returning an
              ;; empty list
              (guix-shell-return '()))
            (begin
              (error-command-failed m)
              mv)))))

(define-inlinable (guix-shell-dry-run-bind mv mf)
  "Does NOT contain (mf lst-params)"
  ;; (format #t "\n")
  ;; (format #t "mv: ~a\n" mv)
  ;; (format #t "mf: ~a; (list? mf): ~a\n" mf (list? mf))
  (if (and (list? mf) (equal? (car mf) 'override-mv))
      (begin
        ;; (format #t "mf: (cdr mf): ~a\n" (cdr mf))
        (guix-shell-return (cdr mf)))
      (let* [(lst-output-of-previous-cmd (car mv))
             (lst-params (cadr mv))]
        (if (equal? lst-output-of-previous-cmd lst-params)
            (begin
              (format #t "~s… " `(,mf ,lst-params))
              ;; (format #t "~s…\n" `(,mf ,lst-params))
              ;; (mf lst-params)
              ;; (format #t "~s… done\n" `(,mf ,lst-params))
              (format #t "done\n")
              ;; enforce manual command params specification by returning an
              ;; empty list
              (guix-shell-return '()))
            (begin
              (error-command-failed m)
              mv)))))

(define-monad compose-commands-guix-shell-dry-run
  (bind guix-shell-dry-run-bind)
  (return guix-shell-return))

(define-monad compose-commands-guix-shell
  (bind guix-shell-bind)
  (return guix-shell-return))

(define-public (mdelete-file prms)
  (let [(file (car prms))]
    (when (access? file F_OK)
      (delete-file file))))

(define-public (mcopy-file prms) (apply copy-file prms))

(define-public (string-in? lst string-elem)
  "Return the first element of @var{lst} that equals (string=)
@var{string-elem}, or @code{#f} if no such element is found.

(string-in? (list \"a\" \"b\" \"c\") \"b\") ; => \"b\"
(string-in? (list \"a\" \"b\" \"c\") \"X\") ; => #f
(string-in? (list \"a\" \"b\" \"c\") \"\")  ; => #f
(string-in? (list \"a\" \"b\" \"c\") #f)    ; => Exception

Requires:
  (use-modules (srfi srfi-1))"
  (find (lambda (e) (string= string-elem e)) lst)
  ;; Alternative implementation
  #;
  (if-let [r (member string-elem lst)]
    (car r)))

#|
(use-modules (ice-9 exceptions))
;;;
(define (disk-space-amount) 1000)
(define (disk-space-left? query) (< query (disk-space-amount)))
;;;
(define-exception-type &read-exception &exception make-read-exception read-exception?
                                        ; (field-name field-accessor) ...
  (read-reason read-exception-reason)
  (read-severity read-exception-severity))
;;;
(with-exception-handler
    (lambda (exception)
      (cond
       ((and (read-exception? exception)
             (eq? (read-exception-reason exception)  'almost-full))
        (format #t "the disk is almost full, only has ~a left.\n"
                (disk-space-amount))
        (format #t "please provide a different file size: ")
        (let ((new-file-size (read)))
          (if (disk-space-left? new-file-size)
              new-file-size
              (raise-exception exception))))
       (else (raise-exception exception))))
  (lambda ()
    (let ((file-size (if (disk-space-left? 1028)
                         1028
                         (raise-continuable
                          (make-read-exception 'almost-full 'medium)))))
      (format #t "writing ~a\n" file-size))))
|#

(define-public (package-output-paths one-or-more-packages)
  "Usage example:
(package-output-paths (@(gnu packages emacs) emacs))
=> (\"/gnu/store/09a50cl6ndln4nmp56nsdvn61jgz2m07-emacs-29.1\")"
  (let [(connection ((@(guix store) open-connection)))]
    (map (comp
          ;; (partial format #f "~a/bin/emacs")
          (@(guix derivations) derivation->output-path)
          (partial (@(guix packages) package-derivation)
                   connection))
         (ensure-list one-or-more-packages))))

(define-public (interleave . lists)
  "Take elements alternately from each list, stopping at the shortest."
  (apply append
         (apply map list lists)))

(define-public (combine . lists)
  "(combine (list 1 2 3) (list 4 5 6)) ;=> ((1 4) (2 5) (3 6))"
  (let ((len (length (car lists))))
    (unless (every (λ (l) (= (length l) len)) lists)
      (error "combine: lists must all be the same length" lists))
    (apply map list lists)))

(define-public (keyword->string keyword)
  "
(use-modules (srfi srfi-88))
(keyword->string #:example) ; => \"example\"
"
  (symbol->string (keyword->symbol keyword)))

;; (define-public (inferior-package-in-guix-channel package commit)
;;   "Returns an inferior representing the `commit' (predecessor-sha1) revision.
;; Can't be in the guix/common/utils.scm. Therefore duplicated.
;; See guix/manifest-emacs-29.1.scm, guix/home/common/config/packages/all.scm"
;;   (first
;;    (lookup-inferior-packages
;;     (inferior-for-channels
;;      (list (channel
;;             (name 'guix)
;;             (url "https://git.savannah.gnu.org/git/guix.git")
;;             (commit commit))))
;;     package)))
;; (testsymb 'inferior-package-in-guix-channel)

(define-public (directory-exists? dir)
  "Return #t if DIR exists and is a directory. From $dgx/guix/build/utils.scm"
  (let ((s (stat dir #f)))
    (and s
         (eq? 'directory (stat:type s)))))

;; (define-public (directory-exists? path)
;;   "Check if path exists and is a directory. (Alternative definition)"
;;   (and (file-exists? path)
;;        (eq? (stat:type (stat path)) 'directory)))

;;; take and drop are in (use-modules (srfi srfi-1))
(define-public (take-smart a b)
  "Accepts either (number list) or (list number)"
  (cond ((and (number? a) (list? b)) (take b a))
        ((and (list? a) (number? b)) (take a b))
        (else (error "take-smart: expected one number and one list" a b))))

(define-public (drop-smart a b)
  "Accepts either (number list) or (list number)"
  (cond ((and (number? a) (list? b)) (drop b a))
        ((and (list? a) (number? b)) (drop a b))
        (else (error "drop-smart: expected one number and one list" a b))))

(define-public (take-last n lst)
  "Returns the last n elements of lst."
  (let ((len (length lst)))
    (if (>= n len)
        lst
        (drop lst (- len n)))))

(define-public (take-last-smart a b)
  "Accepts either (number list) or (list number)"
  (cond ((and (number? a) (list? b)) (take-last a b))
        ((and (list? a) (number? b)) (take-last b a))
        (else (error "take-last-smart: expected one number and one list" a b))))

(define-public (drop-last n lst)
  "Returns lst without its last n elements."
  (take lst (- (length lst) n)))

(define-public (drop-last-smart a b)
  "Accepts either (number list) or (list number)"
  (cond ((and (number? a) (list? b)) (drop-last a b))
        ((and (list? a) (number? b)) (drop-last b a))
        (else (error "drop-last-smart: expected one number and one list" a b))))

(define-public (butlast lst)
  "Returns lst without its last element."
  (drop-last 1 lst))

(define-public (butlast-smart . args)
  "With one argument, it behaves like butlast.\n

With two arguments, it accepts either (number list) or (list number) and drops
that many from the end."
  (cond ((null? args)
         (error "butlast-smart: no arguments provided"))
        ((= (length args) 1)
         (let ((arg (car args)))
           (if (list? arg)
               (butlast arg)
               (error "butlast-smart: single argument must be a list" arg))))
        ((= (length args) 2)
         (let ((a (car args)) (b (cadr args)))
           (cond ((and (number? a) (list? b)) (drop-last a b))
                 ((and (list? a) (number? b)) (drop-last b a))
                 (else (error "butlast-smart: expected one number and one list" a b)))))
        (else (error "butlast-smart: expected 1 or 2 arguments"))))

(define-public (cartesian xs ys)
  "(cartesian '(a b) '(1 2)) => ((a 1) (a 2) (b 1) (b 2))"
  (apply append
         (map (lambda (x)
                (map (lambda (y) (list x y)) ys))
              xs)))

(define-public (member? x lst) (boolean (member x lst)))

;; Both implementations of `if-not` are equivalent. The one done with
;; `define-syntax` is here for future extensibility
;; (define-syntax-rule (if-not condition then else)
;;   (if (not condition) then else))

(define-syntax if-not
  (syntax-rules ()
    ((_ test then else)
     (if (not test) then else))))

(define-public (syntax->list orig-ls)
  "From $der/racket/pkgs/racket-benchmarks/tests/racket/benchmarks/common/psyntax-input.txt

(syntax->list (call-with-input-string \"  (+ 1 2)\" read-syntax))
=> (#<syntax:unknown file:1:3 +> #<syntax:unknown file:1:5 1> #<syntax:unknown file:1:7 2>)
"
  (let f ((ls orig-ls))
    (syntax-case ls ()
      (() '())
      ((x . r) (cons (syntax x) (f (syntax r))))
      (_ (error 'syntax->list "invalid argument ~s" orig-ls)))))

(define (build one-or-more-packages)
  "Usage example:
(build (@(bost gnu packages emacs-xyz) emacs-tweaks))"
  (let [(daemon ((@ (guix store) open-connection)))]
    ;; Define `partial' locally so that this procedure is self-sustained
    (define (partial fun . args) (lambda x (apply fun (append args x))))
    (define (ensure-list args) (if (list? args) args (list args)))
    (map (compose
          ;; (lambda (p) (format #t "3 p: ~a\n" p) p)
          (partial (@ (guix derivations) build-derivations) daemon)
          ;; (lambda (p) (format #t "2 p: ~a\n" p) p)
          list
          ;; (lambda (p) (format #t "1 p: ~a\n" p) p)
          (partial (@ (guix packages) package-derivation) daemon)
          ;; (lambda (p)
          ;;   (format #t "0 p: ~a\n" p)
          ;;   (format #t "(record? p): ~a\n" (record? p))
          ;;   (format #t "(package? p) p: ~a\n" (package? p))
          ;;   p)
          )
         (ensure-list one-or-more-packages))

    ;; ((compose
    ;;   (lambda (p) (format #t "3 p: ~a\n" p) p)
    ;;   (partial (@ (guix derivations) build-derivations) daemon)
    ;;   (lambda (p) (format #t "2 p: ~a\n" p) p)
    ;;   list
    ;;   (lambda (p) (format #t "1 p: ~a\n" p) p)
    ;;   (partial (@ (guix packages) package-derivation) daemon)
    ;;   (lambda (p)
    ;;     (format #t "0 p: ~a\n" p)
    ;;     (format #t "(record? p: ~a\n" (record? p))
    ;;     (format #t "(package? p) p: ~a\n" (package? p))
    ;;     p)
    ;;   )
    ;;  (specification->package
    ;;   (format #f "(@ (bost packages emacs-xyz) ~a)" (symbol->string one-or-more-packages))
    ;;   ))
    ))

(define (symbolic-link? path)
  "Check if path is a symbolic link"
  (and (file-exists? path)
       (eq? (stat:type (lstat path)) 'symlink)))

(define-public split-on-whitespace string-tokenize)
;; (split-on-whitespace "a b\tc\nd") => ("a" "b" "c" "d")

(define-public (pr-str . xs)
  "Return a string containing the printed representation of all arguments,
separated by spaces.

Example:
(pr-str 1 '(2 3) 'x \"foo\") ; => \"1 (2 3) x \\\"foo\\\"\"
"
  (string-join (map (lambda (x) (object->string x)) xs) " "))

(define-public (pr-str-with-quote . xs)
  "See `pr-str'.

Example:
(pr-str-with-quote 1 2 '(3 4) \"foo\") ; => \"1 '(2 3) 'x \\\"foo\\\"\"
"
  ((comp
    (lambda (lst) (string-join lst " "))
    (partial map (lambda (x)
                   (str
                    (cond
                     [(symbol? x) "'"]
                     [(or (list? x) (pair? x)) "'"]
                     [#t ""])
                    (object->string x)))))
   xs))

(define (safe-write-to-file mode filename text)
  "Append TEXT to FILENAME. Create FILENAME if it doesn't exist."
  (guard
      (condition
       (else
        (let [(errmsg
               (format #f "~a: ~a"
                       (exception-origin condition)
                       (apply (partial format #f (exception-message condition))
                              (exception-irritants condition))))]
          (error errmsg))))
    (let [(port (open-file filename mode))]
      (display text port)
      (close-port port)
      #t)))

(define-public (safe-write-append filename text)
  (safe-write-to-file "a" filename text))
(testsymb 'safe-write-append)

(define-public (safe-overwrite filename text)
  (safe-write-to-file "w" filename text))
(testsymb 'safe-overwrite)

(define (read-mounts)
  (call-with-input-file "/proc/mounts"
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (get-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (usb-device? dev)
  "Return #t if DEV (like /dev/sdb1) is backed by a USB device."
  (let* ((basename (basename dev))
         ;; Resolve symlink in /sys/class/block to see where it points
         (sys-path (string-append "/sys/class/block/" basename)))
    (and (file-exists? sys-path)
         (let ((target (false-if-exception (readlink sys-path))))
           (and target (string-contains target "usb"))))))

(define (mounted-usb-devices)
  "Return a list of mounted USB block devices (e.g. /dev/sdb1)."
  ;; (define f (format #f "~a [mounted-usb-devices]" m))
  ;; (format #t "~a Starting…\n" f)
  (let* [(cmd-result-struct
          ((comp
            (lambda (cmd) (exec cmd #:return-plist #t))
            cmd->string)
           (list "findmnt --real --raw --noheadings --output SOURCE")))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        ((comp
          ;; (lambda (p) (format #t "~a done\n" f) p)
          (partial filter usb-device?)
          ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
          )
         (plist-get cmd-result-struct #:results))
        (begin
          ;; error-out
          (error (format #f "~a retcode: ~a\n" m retcode))
          ))))

(define-public (get-ethernet-interfaces)
  ;; (define f (format #f "~a [get-ethernet-interfaces]" m))
  ;; (format #t "~a Starting…\n" f)
  (let* [(cmd-result-struct
          ((comp
            (lambda (cmd) (exec cmd #:return-plist #t))
            cmd->string)
           (list "grep -l '1' /sys/class/net/*/type | cut -d'/' -f5")))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        ((comp
          ;; (lambda (p) (format #t "~a done\n" f) p)
          ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
          )
         (plist-get cmd-result-struct #:results))
        (begin
          ;; error-out
          (error (format #f "~a retcode: ~a\n" m retcode))))))

(define-public (ethernet-cable-plugged? iface)
  "Returns #t or #f"
  (catch #t
    (lambda ()
      (call-with-input-file
          (string-append "/sys/class/net/" iface "/carrier")
        (lambda (port)
          (string=? "1\n" (get-string-all port)))))
    (lambda (key . args)
      #f)))

(define (mounted-with-option? option device-or-mountpoint)
  "Return #t if the given DEVICE-OR-MOUNTPOINT is mounted with specified OPTION.
Example usage:
(mounted-with-option? \"rw\" \"/run/media/bost/lbl-fsys-axagon\")
(mounted-with-option? \"ro\" \"/dev/sdc1\")
"
  (define f (format #f "~a [mounted-with-option?]" m))

  ;; (format #t "~a Starting…\n" f)
  (let* [(cmd-result-struct
          ((comp
            (lambda (cmd) (exec cmd #:return-plist #t))
            cmd->string)
           (list "findmnt --real --noheadings --output OPTIONS"

                 ;; Explicitly define the mount source. Supported specifications are:
                 ;; device
                 ;; maj:min
                 ;; LABEL=label
                 ;; UUID=uuid
                 ;; PARTLABEL=label
                 ;; PARTUUID=uuid
                 ;; "--source"

                 device-or-mountpoint)))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        ((comp
          ;; (lambda (p) (format #t "~a done\n" f) p)
          (partial member option)
          (lambda (the-str) (string-split the-str #\,))
          string-trim-both
          car
          ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
          )
         (plist-get cmd-result-struct #:results))
        (begin
          ;; error-out
          (error (format #f "~a retcode: ~a\n" m retcode))))))

(define-public (writeable-usb-mounted?)
  ((comp
    (partial find true?)
    (partial map boolean)
    (partial map (partial mounted-with-option? "rw")))
   (mounted-usb-devices)))

(define-public (sha1-string s)
  "Example:
  (sha1-string \"0200000000010171\")
;; => \"e2462d5e457858930952c8b7b80f49f3307234ec\"

See also:
  (string-hash \"0200000000010171\")     ; => 1902129584164781890
  (hash \"0200090000010170\" 2147483647) ; => 626328076"
  (let* [(cmd-result-struct
          ((comp
            (lambda (cmd) (exec cmd #:return-plist #t #:verbose #f))
            cmd->string)
           (list (string-append "echo -n '" s "' | sha1sum"))))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        ((comp
          car
          (lambda (s) (string-split s #\space))
          car
          ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
          )
         (plist-get cmd-result-struct #:results))
        (begin
          ;; error-out
          (error (format #f "~a retcode: ~a\n" m retcode))
          ;; (error-command-failed m (format #f "retcode: ~a" retcode))
          ;; *unspecified*
          ))))

(define-public (string-checksum s)
  "(string-checksum \"0200000000010171\") ; => 683979683"
  ;; simple polynomial hash: sum over chars of (char-code * weight^i) mod some
  ;; modulus
  (let* ((modulus 1000000007)   ; a large prime
         (base 257)
         (len (string-length s)))
    (let loop ((i 0) (acc 0))
      (if (= i len)
          acc
          (let* ((c (char->integer (string-ref s i)))
                 (acc2 (modulo (+ (modulo (* acc base) modulus)
                                  c)
                               modulus)))
            (loop (+ i 1) acc2))))))

(define* (timestamp #:key (verbose #f))
  "(timestamp) ;; => \"2025-10-14_20-14-16\""
  (let* [(cmd-result-struct
          ((comp
            (lambda (cmd) (exec cmd #:return-plist #t #:verbose verbose))
            cmd->string)
           (list
            ;; same as "+%Y-%m-%d_%H-%M-%S"
            "date" "\"+%F_%H-%M-%S\"")))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        ((comp
          ;; car
          ;; (lambda (s) (string-split s #\space))
          car
          ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
          )
         (plist-get cmd-result-struct #:results))
        (begin
          ;; error-out
          (error (format #f "~a retcode: ~a\n" m retcode))
          ;; (error-command-failed m (format #f "retcode: ~a" retcode))
          ;; *unspecified*
          ))))

(define* (sha1-file filename #:key (verbose #f))
  "Example:
  (sha1-file \"/etc/hosts\") ;; => \"...\""
  (let* [(cmd-result-struct
          ((comp
            (lambda (cmd) (exec cmd #:return-plist #t #:verbose verbose))
            cmd->string)
           (list "sha1sum" filename)))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        ((comp
          car
          (lambda (s) (string-split s #\space))
          car
          ;; (lambda (v) (format #t "~a 0: ~a\n" m v) v)
          )
         (plist-get cmd-result-struct #:results))
        (begin
          ;; error-out
          (error (format #f "~a retcode: ~a\n" m retcode))
          ;; (error-command-failed m (format #f "retcode: ~a" retcode))
          ;; *unspecified*
          ))))

(define-public inc 1+)

(define* (str-join ls #:optional (delimiter " ") (grammar 'infix))
  "
(str-join (map str (list 1 2 3)))         ;=> \"1 2 3\"
(str-join (map str (list 1 2 3)) \"\\n\") ;=> \"1\\n2\\n3\"
(str-join \"_\" (map str (list 1 2)))     ;=> \"1_2\"
(str-join (map str (list 1 2)) \"_\")     ;=> \"1_2\""
  (cond
   [(and (list? ls) (string? delimiter) (symbol? grammar))
    (string-join (map str ls) delimiter grammar)]
   [(and (string? ls) (list? delimiter) (symbol? grammar))
    (string-join (map str delimiter) ls grammar)]
   [else
    (string-join ls delimiter grammar)]))

(define* (map-indexed f seq)
  "(map-indexed (lambda (i x) (list i x)) '(a b c)) ;=> ((0 a) (1 b) (2 c))"
  (cond
   [(list? seq)
    ;; Use the list version
    (let loop ((idx 0) (xs seq) (acc '()))
      (if (null? xs)
          ;; Reverse the accumulator because we built it backwards
          (reverse acc)
          (loop (+ idx 1) (cdr xs)
                (cons (f idx (car xs)) acc))))]
   [(vector? seq)
    ;; For vectors: build a list or build a new vector
    (let* ((n (vector-length seq))
           (out (make-vector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) out)
        (vector-set! out i (f i (vector-ref seq i)))))
    ]
   [else
    (error "map-indexed: unsupported sequence type" seq)]))

(define-public (padding-string max-length a-string)
  (cond
   [(and (number? a-string) (string? max-length))
    (padding-string a-string max-length)]
   [else
    (cond
     [(= max-length (string-length a-string)) a-string]
     [(< max-length (string-length a-string))
      (error (format #f "(< max-length (string-length a-string)). ~a, ~a"
                     (format #f "max-length : ~a" max-length)
                     (format #f "a-string : ~s" a-string)))]
     [else
      ((comp
        (lambda (lst) (string-join lst ""))
        (lambda (len) (make-list len " "))
        (partial - max-length)
        string-length)
       a-string)])]))

(module-evaluated)
