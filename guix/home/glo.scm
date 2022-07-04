(define-module (glo)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1) ;; find
  #;(loops for-loops)
  ;; (srfi srfi-42)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (glo) -s
!#
|#

(define origin-remotes '("origin" "github"))

(define (string-in? lst string-elem)
  "Return the first element of @var{lst} that equals (string=)
@var{string-elem}, or @code{#f} if no such element is found.
Requires:
  (use-modules (srfi srfi-1))"
  (find (lambda (e) (string= string-elem e)) lst))

;; TODO use continuation breakout instead of a global variable
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
