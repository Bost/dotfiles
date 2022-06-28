(define-module (spag)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:export (main))

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
