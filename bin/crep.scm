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

#;(define (dbg prm)
  (format #t "\n~a\n" prm)
  prm)

;; Return a cell (retcode . result)
(define exec
  (compose
   (lambda (cmd)
     (let* ((port (open-input-pipe cmd))
            (result (read-all port)))
       (cons
        (status:exit-val (close-pipe port))
        result)))
   (lambda (cmd) (string-join cmd " "))))

(define (main args)
  ((compose
    display
    (lambda (r) (string-join r "\n"))
    cdr
    exec
    (lambda (files) (append (list "search-notes" "-f" files "-p") (cdr args)))
    (lambda (s) (string-append "\"" "'[" "\\\"" s "\\\"" "]" "\""))
    (lambda (files) (string-join files "\\\" \\\""))
    (partial filter (lambda (files) (not (string-match "utf8" files))))
    cdr
    exec)
   (list
    "ls" "~/dev/notes/org-roam/*.org")))
