(define-module (utils)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #| #:use-module (guix build utils) ;; invoke - not needed |#
  #:export (partial dbg exec)
  )

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define (dbg prm)
  (format #t "\n~a\n" prm)
  prm)

(define (read-all port)
  "Return a list of all lines from the PORT."
  (let loop ((res '())
             (str (read-line port))) ; from (ice-9 popen)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

(define (exec command)
  "Usage:
(let* ((ret (exec command)))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#)
        (format #t \"Command failed\")))"
  ((compose
    (lambda (command)
      (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
             (str  (read-all port)))
        (cons
         (status:exit-val (close-pipe port))
         str)))
    (lambda (s)
      ;; TODO implement pretty-print for bash commands
      ;; ~a - outputs an argument like display
      ;; ~s - outputs an argument like write (i.e. print to string)
      ;; ~% is newline \n
      (format #t "~a~%" s)
      s)
    (lambda (cmd) (if (list? cmd)
                      (string-join cmd " ")
                      cmd)))
   command))
