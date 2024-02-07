;;; This module is required by some of the scm-bin CLI utilities. The output of
;;; the `format' will also appear in the console such a utility is executed.

;; TODO add --dry-run parameter to every exec* command

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
  #:use-module (guix build utils)
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

  ;; TODO seems like it must be added to (srvc scheme-files)
  ;; #:use-module (guix monads)

  #:use-module (rnrs io ports) ;; for the exec-with-error-to-string
  #:export (
            def*
            error-command-failed
            exec-system*
            exec-with-error-to-string
            testsymb
            testsymb-trace
            evaluating-module
            module-evaluated
            )
  #:re-export (
               which ;; from (guix build utils)
               )
  )

(define m "[utils]")
;; (format #t "~a evaluating module ...\n" m)

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
((juxt a b c) x) => (list (a x) (b x) (c x))"
  (lambda args
    (map (lambda (fn) (apply fn args)) fns)))

(define eq-op? string-ci=?)
(define-public (s+ . rest) (apply (partial lset-union eq-op?) rest))
(define-public (s- . rest) (apply (partial lset-difference eq-op?) rest))
(define-public (sx . rest) (apply (partial lset-intersection eq-op?) rest))

;; (warn ...) doesn't print anything
(define (my=warn s)
  ;; (error s)
  (format #t "WARN ~a\n" s))

(define-public (module-name-for-logging)
  ((compose
    (partial format #f "[~a]")
    (partial string-join)
    (partial map (partial format #f "~a"))
    (partial module-name))
   (current-module)))

(unless (equal? (module-name-for-logging) m)
  (format #t "WARN ~a (equal? (module-name-for-logging) m): ~a\n"
          m (equal? (module-name-for-logging) m)))

(define-syntax evaluating-module
  (syntax-rules ()
    [(_ show)
     (begin
       (let [(m (module-name-for-logging))]
         (when show
           (format #t "~a evaluating module ...\n" m))))]
    [(_)
     (begin
       (let [(m (module-name-for-logging))
             (show #f)]
         (when show
           (format #t "~a evaluating module ...\n" m))))]))

(define-syntax module-evaluated
  (syntax-rules ()
    [(_ show)
     (begin
       (let [(m (module-name-for-logging))]
         (when show
           (format #t "~a module evaluated\n" m))))]
    [(_)
     (begin
       (let [(m (module-name-for-logging))
             (show #f)]
         (when show
           (format #t "~a module evaluated\n" m))))]))

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

(define-public (last lst) (car (reverse lst)))

(define-public (pretty-print->string sexp)
  (let [(port (open-output-string))]
    (pretty-print sexp port)
    (let* [(ret (get-output-string port))]
      (close-output-port port)
      ret)))

(define-public (unspecified-or-empty-or-false? obj)
  (or (unspecified? obj)
      (null? obj)
      (and (string? obj) (string-null? obj))
      (eq? #f obj)))

;; Turn the colon-separated PATH-string, into a list and
;; return the resulting list with tail appended
(define-public path
  (delete-duplicates (parse-path (getenv "PATH"))))

(define-public str string-append)

;; TODO see
;; (define s (string-match "[0-9][0-9][0-9][0-9]" "blah2002foo"))
;; (match:end s) ⇒ 8
(define-public (has-suffix? str suf)
  "Returns #t if the given string ends with the given suffix, otherwise or #f."
  (string-suffix? suf str))

(define-public (has-substring? str subs)
  (not (not (string-match subs str))))

(define-public (drop-right xs n)
  "(drop-right (list 1 2 3 4 5) 2) ;; => (1 2 3)
TODO what's the clojure variant?"
  (reverse (list-tail (reverse xs) n)))

(define-public (drop-left xs n)
  "(drop-left (list 1 2 3 4 5) 2) ;; => (4 5)
TODO what's the clojure variant?"
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

(define-public (dbg prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; TODO implement pretty-print for bash commands
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "\n~a\n" prm)
  prm)

(define-public (dbg-exec prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; TODO implement pretty-print for bash commands
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (if (list? prm)
      (format #t "$ ~a\n" (string-join prm))
      (format #t "$ ~a\n" prm))
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

(define-public (string-split-whitespace arg)
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

(define-public (read-all reader-function)
  "Returns a function which reads all lines of text from the PORT and applies
READER-FUNCTION on them. "
  (lambda (port)
    (let loop ((res '())
               (str (reader-function port))) ; from (ice-9 popen)
      (if (and str (not (eof-object? str)))
          (loop (append res (list str))
                (reader-function port))
          res))))

(define-public (read-all-sexprs p)
  (let f ((x (read p)))
    (if (eof-object? x)
        '()
        (cons x (f (read p))))))

(define-public (read-all-syntax port)
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

(define-public (exec-background command)
  "Execute the COMMAND in background, i.e. in a detached process.
COMMAND can be a string or a list of strings."
  ((compose
    system
    dbg-exec
    cmd->string
    (lambda (cmd) (list cmd "&" "disown"))
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

(define (exec-with-error-to-string cmd)
  "Run the shell COMMAND using ‘/bin/sh -c’ with ‘OPEN_READ’ mode, ie. to read
from the subprocess. Wait for the command to terminate and return 3 values:
- `#t' if the port was successfully closed or `#f' if it was already closed.
- a string containing standard output
- a string containing standard error output

Usage:
(receive (retval stdout stderr)
    (exec-with-error-to-string \"echo to-stdout; echo to-stderr >&2\")
  (format #t \"receive retval:~a\\n\" retval)
  (format #t \"receive stdout:~a\\n\" stdout)
  (format #t \"receive stderr:~a\\n\" stderr))
"
  (let* ((err-cons (pipe))
         (port (with-error-to-port (cdr err-cons)
                 (lambda () (open-input-pipe cmd))))
         ;; the err-cons buffer size is 16 MiB
         (_ (setvbuf (car err-cons) 'block (* 1024 1024 16)))
         (stdout (read-delimited "" port)))
    (values
     (close-port (cdr err-cons))
     stdout
     ;; the port must be closed before calling the following
     (read-delimited "" (car err-cons)))))

(define-public (exec command)
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
      ;; Can't use the `call-with-port' since the exit-val is needed.
      (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
             ;; the `read-all-strings' must be called before `close-pipe'.
             (results (read-all-strings port)))
        (cons
         (status:exit-val (close-pipe port))
         results)))
    dbg-exec
    cmd->string)
   command))

(define-public (analyze-pids-flag-variable init-cmd client-cmd pids)
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

(define-public (analyze-pids-call/cc init-cmd client-cmd pids)
  "For a process ID from the list of PIDS, return the INIT-CMD if no process ID was
found or the CLIENT-CMD if some process ID was found."
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

(define-public (compute-cmd init-cmd client-cmd pattern)
  "pgrep for a user and PATTERN and return the INIT-CMD if no process ID was found
or the CLIENT-CMD if some process ID was found."
  ((compose
    (partial
     analyze-pids-call/cc
     ;; analyze-pids-flag-variable
     init-cmd client-cmd)
    cdr
    exec
    ;; --euid effective ID
    (partial format #f "pgrep --full --euid ~a ~a" user)
    (lambda (s) (str "\"" s "\"")))
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

(define-public (mktmpfile)
  ;; (tmpnam) could be used instead of all of this, however I get deprecation
  ;; warning sometimes
  ((compose
    port-filename
    mkstemp!
    ;; prevent the 'string is read-only ...' error
    string-copy)
   "/tmp/myfile-XXXXXX"))

(define-public (ends-with? s postfix)
  "(ends-with? \"/aaa/bbb/ccc/\" \"/\")
 => #t"
  (string-suffix? postfix s))

;; TODO add install-recursively to (guix build utils) and send it to upstream
(define* (install-recursively source destination
                              #:key
                              (log (current-output-port))
                              (follow-symlinks? #f)
                              (copy-file copy-file)
                              keep-mtime? keep-permissions?)
  "Recursive version of install-file."
  (mkdir-p destination)
  (copy-recursively source
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
                            (one-or-more (not (any " ")))
                            string-end))))
    (regexp-match url-regex url)))

(define-public (remove-kw-from-args kw init-args)
  "init-args must be a list containing a sequence key-val pairs. E.g.:
(#:x 'x #:y 'y)"
  (let loop ((args init-args)
             (result '()))
    ;; (format #t "kw: ~a; args: ~a; result: ~a\n" kw args result)
    (cond ((null? args) (reverse result))
          (
           (and
            (equal? kw (car args))
            (>= (length args) 2)
            )
           (begin
             ;; (format #t "Skipping over: ~a\n" (list (car args) (cadr args)))
             (loop
              (cddr args) ;; skip first 2
              result)))

          (else
           (loop
            (cdr args)
            (append (list (car args)) result))))))

;; (define* (fox #:key x y #:rest args)
;;   (format #t "input : args: ~a\n" args)
;;   (let* ((args (remove-kw-from-args #:x args))
;;          (args (remove-kw-from-args #:y args))
;;          )
;;     (format #t "output: args: ~a\n" args)))

;; (fox #:x "x" #:y "y" 'bla)
;; ;; => input : args: (#:x x #:y y bla)
;; ;; => output: args: (bla)


(define-inlinable (pipe-return command)
  (list
   ;; Return code signaling that some hypothetical previous command terminated
   ;; successfully.
   0
   ;; String containing the command to execute as next
   command))

(define-inlinable (pipe-bind mv f)
  (let* ((mv-retcode (car mv)))
    (if (= 0 mv-retcode)
        ;; the f-function parses the output
        (f (cadr mv))
        (begin
          (format #t "~a\n" (error-command-failed))
          mv))))

;; (define-monad compose-shell-commands
;;   (bind pipe-bind)
;;   (return pipe-return))


;; (with-monad compose-shell-commands
;;   (>>= (return "uname -o")
;;        exec
;;        (partial echo #:string)
;;        ))

;; (define x "aaa")
;; (define mv (return x))
;; (define f (partial echo #:string))
;; (define g (partial echo #:string))
;; (proper-monad? mv x f g)

(module-evaluated)
