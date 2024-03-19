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
  #:use-module (guix build utils)
  #:use-module (ice-9 match) ;; match
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
  ;; string-replace-substring
  #:use-module (ice-9 string-fun)
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; return, bind
  #:use-module (guix monads)
  ;; for the exec-with-error-to-string
  #:use-module (rnrs io ports)
  #:export (
            compose-shell-commands
            def*
            error-command-failed
            evaluating-module
            exec-system*
            exec-with-error-to-string
            if-let
            module-evaluated
            testsymb
            testsymb-trace
            ))

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
  (format #t "W ~a\n" s))

(define-public (module-name-for-logging)
  ((comp
    (partial format #f "[~a]")
    (partial string-join)
    (partial map (partial format #f "~a"))
    (partial module-name))
   (current-module)))

(unless (equal? (module-name-for-logging) m)
  (format #t "W ~a (equal? (module-name-for-logging) m): ~a\n"
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

(if-let (result (and #f (some-computation)))
        (format #t "Falsey Test Failed: Should not reach here\n")
        (format #t "Falsey Test Passed: Correctly reached else clause\n"))
|#

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

(define-public (has-suffix? string suffix)
  "Does STRING end with the SUFFIX? As `string-suffix?' but the parameters are
reversed. See also:
(define s (string-match \"[0-9][0-9][0-9][0-9]\" \"blah2002foo\"))
(match:end s) ;; ⇒ 8"
  (string-suffix? suffix string))

(define-public ends-with? has-suffix?)

(define-public (has-substring? str subs)
  (not (not (string-match subs str))))

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

;; TODO implement pretty-print for bash commands
(define-public (dbg prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "\n~a\n" prm)
  prm)

(define-public (dbg-exec prm)
  "`pk', i.e. `peek' can be used instead of this function"
  (if (list? prm)
      (format #t "$ ~a\n" (string-join prm))
      (format #t "$ ~a\n" prm))
  prm)

(define* (error-command-failed #:rest args)
  "Returns #t and prints \"Command failed.\" with some extra text.

(error-command-failed \"[module]\" \"extra_text\")
;; =>
E [module] Command failed: extra_text

(error-command-failed \"[module]\")
;; =>
E [module] Command failed.

(error-command-failed)
;; =>
E Command failed."
  (match args
    ['()
     (format #t
             #;error
             "E Command failed.\n")]
    [(module)
     (format #t "E ~a Command failed.\n" module)]
    [(module extra-text)
     (format #t "E ~a Command failed: ~a\n" module extra-text)]))

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

(define-public (string-split-whitespace arg)
  ((comp
    ;; (partial string-sff #\space)
    flatten (partial map split-space-escaped)
    (partial string-sff #\newline)
    (partial string-sff #\tab)
    (lambda (arg) (if (list? arg) arg (list arg))))
   arg))

#;
(string-split-whitespace
 "gcl  /some/other/path/
  xxx
    yyy
/some/path")

(define dry-run-prm "--gx-dry-run")

(define* (exec-or-dry-run exec-function args)
  (if (or (and (list? args) (member dry-run-prm args))
          (and (string? args) (string-contains args dry-run-prm)))
      args
      (if (list? args)
          (apply exec-function args)
          (exec-function args))))

(define* (exec-system* #:rest args)
  "Execute system command and returns its ret-code. E.g.:
(exec-system* \"echo\" \"bar\" \"baz\") ;; =>
$ (echo bar baz)
bar baz
$9 = 0 ;; return code"
  ((comp
    (partial exec-or-dry-run system*)
    dbg-exec
    string-split-whitespace)
   args))

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

(define-public (exec-background command)
  "Execute the COMMAND in background, i.e. in a detached process.
COMMAND can be a string or a list of strings."
  ((comp
    (partial exec-or-dry-run system)
    dbg-exec
    cmd->string
    ;; disown belongs to shells. See `help disown`. The semicolon, as indicated
    ;; by `help disown` ivoked from the fish-shell, in eg. `echo foo &; disown`,
    ;; doesn't work in bash, only in the fish-shell
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

(define (exec-with-error-to-string commad)
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
    ;; exec-function returns multiple values contains and the exec-or-dry-run is
    ;; able to return only one value.
    exec-function
    dbg-exec
    cmd->string)
   commad))

(define-public (exec command)
  "Run the shell COMMAND using '/bin/sh -c' with 'OPEN_READ' mode, ie. to read
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
        ;; (error-command-failed \"[module]\" \"extra_info\")
        *unspecified*)))"
  ;; ,use (guix build utils) ;; contains `invoke'
  ;; `invoke' does `(apply system* program args)'; `system*' waits for the program
  ;; to finish, The command is executed using fork and execlp.

  ;; TODO write a scheme procedure: chdir str
  ;; Change the current working directory to str. The return value is unspecified.
  (define (exec-function command)
    ;; Can't use the `call-with-port' since the exit-val is needed.
    (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
           ;; the `read-all-strings' must be called before `close-pipe'.
           (results (read-all-strings port)))
      (cons
       (status:exit-val (close-pipe port))
       results)))

  ((comp
    (partial exec-or-dry-run exec-function)
    dbg-exec
    cmd->string)
   command))

(define-public (analyze-pids-flag-variable init-cmd client-cmd pids)
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

(define-public (analyze-pids-call/cc init-cmd client-cmd pids)
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

(define-public (compute-cmd init-cmd client-cmd pattern)
  "pgrep for a user and PATTERN and return the INIT-CMD if no process ID was found
or the CLIENT-CMD if some process ID was found."
  ((comp
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
          (error-command-failed m)
          mv))))

(define-monad compose-shell-commands
  (bind pipe-bind)
  (return pipe-return))

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

(define-public (package-output-path package)
  "(package-output-path (@(gnu packages emacs) emacs))
=> \"/gnu/store/09a50cl6ndln4nmp56nsdvn61jgz2m07-emacs-29.1\""
  ((comp
    ;; (partial format #f "~a/bin/emacs")
    (@(guix derivations) derivation->output-path)
    (partial (@(guix packages) package-derivation)
             ((@(guix store) open-connection))))
   package))

(module-evaluated)
