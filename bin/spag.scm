(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 popen)
             (utils))

(define* (git #:rest args)
  (let ((h (getenv "HOME")))
    (cons* "git"
           (string-append "--git-dir=" h "/.emacs.d/.git")
           (string-append "--work-tree=" h "/.emacs.d")
           args)))

(define (main args)
  (map exec
       (list
        (git "fetch" "--tags" "origin" "develop")
        (git "rebase" "origin/develop" "develop")
        (git "rebase" "develop" "cycle"))))
