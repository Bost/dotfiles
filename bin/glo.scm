(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 popen)
             (srfi srfi-1) ;; find
             #;(loops for-loops)
             ;; (srfi srfi-42)
             )

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define (read-all port)
  "Return a list of all lines from the PORT."
  (let loop ((res '())
             (str (read-line port)))
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

;; TODO see 'push all branches to all remotes'
;; https://stackoverflow.com/a/18674313/5151982

(define exec
  (compose
   (lambda (cmd)
     (let* ((port (open-input-pipe cmd))
            (res (read-all port)))
       (cons
        (status:exit-val (close-pipe port))
        res)))
   (lambda (s) (format #t "\n~a\n" s) s)
   (lambda (cmd) (string-join cmd " "))))

(define (dbg prm)
  (format #t "\n~a\n" prm)
  prm)

(define origin-remotes '("origin" "github"))

(define (string-in? lst string-elem)
  "Return the first element of @var{lst} that equals (string=)
@var{string-elem}, or @code{#f} if no such element is found.
Requires:
  (use-modules (srfi srfi-1))"
  (find (lambda (e) (string= string-elem e)) lst))

;; TODO quick and dirty - use global variable
(define found #f)

(define (main args)
  ((compose
    (partial
     map
     (lambda (remote)
       (if (not found)
           ;; TODO if-let
           (let ((r (string-in? origin-remotes remote)))
             (if r
                 (let* ((cmd (list "git" "fetch" "--tags" r))
                        (ret (exec cmd)))
                   (if (= 0 (car ret))
                       (let* ((cmd (append (list "git" "rebase")
                                           (cdr args)))
                              (ret (exec cmd)))
                         (if (= 0 (car ret))
                             (set! found #t)
                             (begin
                               #;(format #t "Command failed:\n~a\n"
                                       (string-join cmd " "))
                               (cdr ret))))
                       (begin
                         #;(format #t "Command failed:\n~a\n"
                                 (string-join cmd " "))
                         (cdr ret)))))))))
    #;dbg
    (partial filter (lambda (remote) (not (string-match "heroku" remote))))
    cdr
    exec)
   (list
    "git" "remote")))
