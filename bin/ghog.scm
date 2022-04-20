(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 popen)
             (utils))

;; TODO see 'push all branches to all remotes'
;; https://stackoverflow.com/a/18674313/5151982

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
