;;; This module is required by some of the scm-bin CLI utilities. The output of
;;; the `format' will also appear in the console such a utility is executed.

;; TODO create a package installable by `guix install my=utils`
;; See: jaro the resource opener - an alternative to xdg-open
;; https://github.com/isamert/jaro/blob/master/jaro
;; See `guile-build-system'
;; Syntax:
;; (MODULE-NAME [#:select SELECTION]
;;              [#:prefix PREFIX]
;;              [#:renamer RENAMER]
;;              [#:version VERSION-SPEC]) ;; R6RS-compatible version reference
(define-module (utils)
  #:use-module (settings)
  ;; open-input-pipe
  #:use-module (ice-9 popen)
;;; (ice-9 readline) requires `guix install guile-readline'.
  ;; #:use-module (ice-9 readline)
  #:use-module (ice-9 rdelim) ;; read-line
  ;; string-match
  #:use-module (ice-9 regex)
  ;; delete-duplicates
  #:use-module (srfi srfi-1)
  ;; #:use-module (guix build utils) ;; invoke - not needed
  #:use-module (ice-9 pretty-print)

  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)

  #:export (
            analyze-pids-call/cc
            analyze-pids-flag-variable
            cmd->string
            comp
            compute-cmd
            cnt
            dbg
            dbg-exec
            def*
            drop-left
            drop-right
            error-command-failed
            exec
            exec-background
            exec-system*
            flatten
            has-substring?
            has-suffix?
            last
            mktmpfile
            module-name-for-logging
            partial
            path
            pretty-print->string
            read-all
            read-all-sexprs
            read-all-syntax
            s+
            s-
            sx
            str
            string-split-whitespace
            testsymb
            testsymb-trace
            unspecified-or-empty-or-false?
            ))

;; https://github.com/daviwil/dotfiles/tree/master/.config/guix
;; Also (examples)
;; (use-service-modules nix)
;; (use-service-modules desktop xorg)
;; (use-package-modules certs)
;; (use-package-modules shells)

;;;;;; beg: testsymb, testsymb-trace

;; neither `=' nor `eqv?' work
(define eq-op? string-ci=?)
(define (s+ . rest) (apply (partial lset-union eq-op?) rest))
(define (s- . rest) (apply (partial lset-difference eq-op?) rest))
(define (sx . rest) (apply (partial lset-intersection eq-op?) rest))

(define cnt length+)

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define (comp . fns)
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

;; (warn ...) doesn't print anything
(define (my=warn s)
  ;; (error s)
  (format #t "WARN ~a\n" s))

(define (module-name-for-logging)
  ((compose
    (partial format #f "[~a]")
    (partial string-join)
    (partial map (partial format #f "~a"))
    (partial module-name))
   (current-module)))

(define-syntax testsymb
  (syntax-rules ()
    [(_ symbol)
     (begin
       (let [(module (module-name-for-logging))]
         (unless (defined? symbol)
           (my=warn (format #f "~a Symbol undefined: ~a" module symbol)))))]))

(define-syntax testsymb-trace
  (syntax-rules ()
    [(_ symbol)
     (begin
       (let [(module (module-name-for-logging))]
         (if (defined? symbol)
             (format #t "~a Symbol defined: ~a\n" module symbol)
             (my=warn (format #f "~a Symbol undefined: ~a" module symbol)))))]))

(define (test-testsymb)
  (define f 42)
  (testsymb 'f)
  (testsymb-trace 'f)
  )

;;;;;; end: testsymb, testsymb-trace

;;; testsymb doesn't work in the let-syntax
;; (let [(ff 42)] (testsymb 'ff))

(define (last lst) (car (reverse lst)))

(define (pretty-print->string sexp)
  (let [(port (open-output-string))]
    (pretty-print sexp port)
    (let* [(ret (get-output-string port))]
      (close-output-port port)
      ret)))

(define (unspecified-or-empty-or-false? obj)
  (or (unspecified? obj)
      (null? obj)
      (and (string? obj) (string-null? obj))
      (eq? #f obj)))

;; Turn the colon-separated PATH-string, into a list and
;; return the resulting list with tail appended
(define path (delete-duplicates
              (parse-path (getenv "PATH"))))

(define str string-append)

;; TODO see
;; (define s (string-match "[0-9][0-9][0-9][0-9]" "blah2002foo"))
;; (match:end s) ⇒ 8
(define (has-suffix? str suf)
  "Returns #t if the given string ends with the given suffix, otherwise or #f."
  (define len-str (string-length str))
  (define len-suf (string-length suf))
  (if (>= len-str len-suf)
      (string=? (substring str (- len-str len-suf) len-str) suf)
      #f))

(define (has-substring? str subs)
  (not (not (string-match subs str))))

(define (drop-right xs n)
  "(drop-right (list 1 2 3 4 5) 2) ;; => (1 2 3)
TODO what's the clojure variant?"
  (reverse (list-tail (reverse xs) n)))

(define (drop-left xs n)
  "(drop-left (list 1 2 3 4 5) 2) ;; => (4 5)
TODO what's the clojure variant?"
  (reverse (list-head (reverse xs) n)))

(define (flatten x)
  "(flatten (list (cons 1 (cons 2 3))))
   ;; => (1 2 3)
   (equal? (list 1 2 3)
           (flatten (list (cons 1 (cons 2 3)))))
   ;; => #t
"
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define (dbg prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; TODO implement pretty-print for bash commands
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "\n~a\n" prm)
  prm)

(define (dbg-exec prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; TODO implement pretty-print for bash commands
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "$ ~a\n" prm)
  prm)

(define* (error-command-failed #:rest args)
  (format #t
          #;error
          "[ERR] Command failed\n"))

(define (string-sff ch s-list)
  ((compose
    (partial filter (compose not string-null?))
    flatten
    (partial map (lambda (s) (string-split s ch))))
   s-list))

(define (string-split-whitespace arg)
  ((compose
    (partial string-sff #\space)
    (partial string-sff #\newline)
    (partial string-sff #\tab)
    (lambda (arg)
      (if (list? arg) arg (list arg))))
   arg))

#;
(string-split-whitespace
 "gcl  /some/other/path/
  xxx
    yyy
/some/path")

(define* (exec-system* #:rest args)
  "Execute system command and returns its ret-code. E.g.:
(exec-system* \"echo\" \"bar\" \"baz\") ;; =>
$ (echo bar baz)
bar baz
$9 = 0 ;; return code"
  ((compose
    (partial apply system*)
    dbg-exec
    string-split-whitespace)
   args))

(define (read-all reader-function)
  "Returns a function which reads all lines of text from the PORT and applies
READER-FUNCTION on them. "
  (lambda (port)
    (let loop ((res '())
               (str (reader-function port))) ; from (ice-9 popen)
      (if (and str (not (eof-object? str)))
          (loop (append res (list str))
                (reader-function port))
          res))))

(define (read-all-sexprs p)
  (let f ((x (read p)))
    (if (eof-object? x)
        '()
        (cons x (f (read p))))))

(define (read-all-syntax port)
  "Return a list of all lines from the PORT."
  (let loop ((res '())
             (str (read-syntax port))) ; from (ice-9 popen)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-syntax port))
        res)))

(define (read-all-strings port)
  "Return a list of all lines of text from the PORT.
Returns a list of strings"
  (let loop ((res '())
             (str (read-line port))) ; from (ice-9 rdelim)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

(define (cmd->string cmd)
  (dbg-exec
   (if (list? cmd)
       (string-join cmd) ;; join with ' ' by default
       cmd)))

;; 8sync https://www.gnu.org/software/8sync/
;; asynchronous programming library for GNU Guile. Based on the actor
;; model, it makes use of delimited continuations to avoid a mess of callbacks
;; resulting in clean, easy to read non-blocking code.
#;(import (language wisp spec)) ;; whitespace lisp

(define (exec-background command)
  "Execute the COMMAND in background, i.e. in a detached process.
"
  ((compose close-port
            open-input-pipe
            cmd->string)
   command))

#;
(define (background-system command)
  " https://sourceware.org/legacy-ml/guile/1998-09/msg00228.html "
  (let ((child-pid (primitive-fork)))
    (if (zero? child-pid)
        ;; Okay, we're the child process.  We need to catch any and
        ;; all errors and exit, or else we'll end up with two Guile
        ;; repls trying to read from the same terminal.
        (begin
          (catch #t
            (lambda ()
              ;; Put ourselves in our own process group.
              (setpgid (getpid) (getpid))
              ;; Try to execute the user's command.
              (execl "/bin/sh" "sh" "-c" command))
            (lambda args #f))
          ;; If we return from the exec for any reason, it means it failed.
          (quit 1))
        ;; Okay, we're the parent process.  Return the child pid, in
        ;; case we want to wait for it at some point in the future.
        child-pid)))

(define (exec command)
  "Run the shell COMMAND using ‘/bin/sh -c’ with ‘OPEN_READ’ mode, ie. to read
from the subprocess. Wait for the command to terminate and return a string
containing its output.

TODO have a look if a delimited continuation can be used to break out of `exec',
i.e. skip the `read-all-strings' and thus make `exec-background' out of it.

Usage:
(let* ((ret (exec command)))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#)
      (begin
        (format #t \"~a\\n\" (error-command-failed))
        *unspecified*)))"
  ;; ,use (guix build utils) ;; contains `invoke'
  ;; `invoke' does `(apply system* program args)'; `system*' waits for the program
  ;; to finish, The command is executed using fork and execlp.

  ;; TODO
  ;; Scheme Procedure: chdir str
  ;; Change the current working directory to str. The return value is unspecified.

  ((compose
    (lambda (command)
      (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
             (str  (read-all-strings port)))
        (cons
         (status:exit-val (close-pipe port))
         str)))
    cmd->string)
   command))

(define (analyze-pids-flag-variable init-cmd client-cmd pids)
  "Breakout implementation using a flag variable"
  (let [(ret-cmd init-cmd)]
    ((compose
      (lambda (p)
        (if (null? p)
            init-cmd ;; No such binary has been started yet.
            (car p)))
      (partial
       map
       (lambda (pid)
         (when (string=? ret-cmd init-cmd) ;; check the flag
           (let ((proc-user ((compose
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

(define (analyze-pids-call/cc init-cmd client-cmd pids)
  (call/cc
   (lambda (continuation)
     (map
      (lambda (pid)
        (let ((proc-user ((compose
                           cadr
                           exec)
;;; '-o' user defined format, 'h' no header, '-p' pid
                          (format #f "ps -o user= h -p ~a" pid))))
          (when (and (not (string-null? proc-user))
                     (string=? user proc-user))
            (let ((proc-cmd ((compose cadr exec)
;;; '-o' user defined format, 'h' no header, '-p' pid
                             (format #f "ps -o command= h -p ~a" pid))))
              (unless (string-match ".*<defunct>$" proc-cmd)
;;; Terminate the call/cc statement with the return value `client-cmd'
                (continuation client-cmd))))))
      pids)
     ;; The pids-list is empty. No such binary has been started yet.
     init-cmd)))

(define (compute-cmd init-cmd client-cmd pattern)
  ((compose
    (partial
     analyze-pids-call/cc
     ;; analyze-pids-flag-variable
     init-cmd client-cmd)
    cdr
    exec
    (partial format #f "pgrep --full -u ~a ~a" user))
   pattern))

(define-syntax def*
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) b0 b1 ...)
       #'(begin
         (format #t "(def* (~a ...) ...) ... " `id)
         (define id (lambda* args b0 b1 ...))
         (format #t "done\n")
         id))

      ((_ id val) (identifier? #'id)
       #'(begin
         (format #t "(def* ~a ...) ... " `id)
         (define id val)
         (format #t "done\n")
         id))
      )))

;; from /home/bost/dev/guile/module/ice-9/psyntax.scm
;; (define-syntax define*
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ (id . args) b0 b1 ...)
;;        #'(define id (lambda* args b0 b1 ...)))
;;       ((_ id val) (identifier? #'id)
;;        #'(define id val)))))

(define-syntax def-public
  (syntax-rules ()
    ((_ (name . args) . body)
     (begin
       (format #t "(def-public (~a ...) ...) ... " `name)
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

(define (mktmpfile)
  ;; (tmpnam) could be used instead of all of this, however I get deprecation
  ;; warning sometimes
  ((compose
    port-filename
    mkstemp!
    ;; prevent the 'string is read-only ...' error
    string-copy)
   "/tmp/myfile-XXXXXX"))

(define (repl)
  (use-modules (utils))
  (load (string-append (getenv "dotf") "/guix/home/fs-utils.scm"))
  )

;; (format #t "[utils] module evaluated\n")
