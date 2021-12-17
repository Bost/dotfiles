(use-modules (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 regex)
             (srfi srfi-1) ;; fold
             #;(language cps intmap))

(define (get-type o)
  "TODO implement: 1 is a number and an integer in the same type"
  (cond
   ((port? o) 'port)
   ((boolean? o) 'boolean)
   ((string? o) 'string)
   ((symbol? o) 'symbol)
   ((list? o) 'list)
   ((vector? o) 'vector)
   ((procedure? o) 'procedure)
   ((complex? o) 'complex)
   ((real? o) 'real)
   ((integer? o) 'integer)
   ((number? o) 'number)
   (#t 'unknown-type)))

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

(define (main args)
  ((compose
  (partial format #t "~a\n")
  (partial map
           (compose
            (lambda (cmd)
              (let* ((port (open-input-pipe cmd))
                     (res (read-all port)))
                (close-pipe port)
                res))
            (lambda (s)
              ;; TODO implement pretty-print for bash commands
              ;; ~a - outputs an argument like display
              ;; ~s - outputs an argument like write (i.e. print to string)
              (format #t "\n~a\n\n" s)
              s)
            (lambda (cmd) (string-join cmd " "))))
  (partial map (lambda (remote)
                 (append
                  (list "git" "push" "--follow-tags" "--verbose" remote)
                  (cdr args))))
  (partial fold-right (lambda (a d) (if (string? a) (cons a d) d)) '())
  (partial map
           (compose
            (lambda (match-structure) (if match-structure
                                          (match:substring match-structure 1)))
            (partial string-match "remote\\.(.*?)\\.url")))
  (lambda (cmd)
    (let* ((port (open-input-pipe cmd))
           (res (read-all port)))
      (close-pipe port)
      res))
  (lambda (s)
    ;; TODO implement pretty-print for bash commands
    ;; ~a - outputs an argument like display
    ;; ~s - outputs an argument like write (i.e. print to string)
    (format #t "\n~a\n\n" s)
    s)
  (lambda (cmd) (string-join cmd " ")))
 (list
  "git" "config" "--local" "--list")))
