(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 popen))

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

(define (main args)
  ((compose
    (partial
     map
     (compose
      cdr
      exec
      (lambda (remote) (append
                        (list "git" "push" "--follow-tags" "--verbose" remote)
                        (cdr args)))
      car))
    (partial filter (lambda (remote-url)
                      (not (null? (cdr remote-url)))))
    (partial map
             (lambda (remote)
               (cons remote
                     ((compose
                       (partial filter (lambda (url) (string-match "git@" url)))
                       cdr
                       exec
                       (partial list "git" "remote" "get-url"))
                      remote))))
    (partial filter (lambda (remote) (not (string-match "heroku" remote))))
    cdr
    exec)
   (list
    "git" "remote")))
