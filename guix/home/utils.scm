#|
(use-modules
 (ice-9 rdelim)
 (ice-9 regex)
 (ice-9 popen))
|#

;; Syntax:
;; (MODULE-NAME [#:select SELECTION]
;;              [#:prefix PREFIX]
;;              [#:renamer RENAMER]
;;              [#:version VERSION-SPEC]) ;; R6RS-compatible version reference
(define-module (utils)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen) #| read-line open-input-pipe |#
  #| #:use-module (guix build utils) ;; invoke - not needed |#
  #:export (flatten partial dbg
            string-split-whitespace
            cmd->string
            read-all-sexprs
            exec
            exec-system*
            exec-background
            home str
            error-command-failed
            drop-right drop-left
            ))

;; https://github.com/daviwil/dotfiles/tree/master/.config/guix
;; Also (examples)
;; (use-service-modules nix)
;; (use-service-modules desktop xorg)
;; (use-package-modules certs)
;; (use-package-modules shells)

(define home (getenv "HOME"))

(define str string-append)

(define* (error-command-failed #:rest args)
  (format #t
   #;error
   "[ERR] Command failed"))

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

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define (dbg prm)
  "`pk' can be used instead of `dbg'"
  ;; TODO implement pretty-print for bash commands
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "~%~a~%" prm)
  prm)

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
  "Usage:
(exec-system* \"echo\" \"bar\" \"baz\")"
  ((compose
    (partial apply system*)
    dbg
    string-split-whitespace)
   args))

(define (read-all-sexprs p)
  (let f ((x (read p)))
    (if (eof-object? x)
        '()
        (cons x (f (read p))))))

(define (read-all-strings port)
  "Return a list of all lines of text from the PORT.
Returns a list of strings"
  (let loop ((res '())
             (str (read-line port))) ; from (ice-9 popen)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

(define (cmd->string cmd)
  (dbg
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
        (format #t \"Command failed\")))"
  ;; ,use (guix build utils) contains `invoke'
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
