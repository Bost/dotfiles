(define-module (spag)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:export (main git-spacemacs))

#|

#!/usr/bin/env -S guile \\
-l utils.scm -e (spag) -s
!#

|#

(define* (git-spacemacs #:rest args)
  (let ((h (getenv "HOME")))
    (cons* "git"
           (string-append "--git-dir=" h "/.emacs.d/.git")
           (string-append "--work-tree=" h "/.emacs.d")
           args)))

(define (main args)
  (map exec
       (list
        (git-spacemacs "fetch" "--tags" "origin" "develop")
        (git-spacemacs "rebase" "origin/develop" "develop")
        (git-spacemacs "rebase" "develop" "cycle"))))
