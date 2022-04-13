(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 popen))

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

;; TODO see 'push all branches to all remotes'
;; https://stackoverflow.com/a/18674313/5151982

(define (read-all port)
  "Return a list of all lines from the PORT."
  (let loop ((res '())
             (str (read-line port)))
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

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

(define* (git #:rest args)
  (cons* "git" (string-append "--work-tree=" (getenv "HOME") "/.emacs.d")
         args))

(define (main args)
  (map exec
       (list
        (git "checkout" "develop")
        (git "fetch" "--tags" "origin")
        (git "rebase")
        (git "rebase" "develop" "cycle"))))
