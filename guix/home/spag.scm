(define-module (spag)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:export (main))

#|
#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (spag) -s
!#
|#

(define* (git #:rest args)
  ;; see also ~/.emacs-profiles.el
  (let ((dist-dir (string-append (getenv "HOME") "/.spacemacs.d")))
    (cons* "git"
           (string-append "--git-dir=" dist-dir "/.git")
           (string-append "--work-tree=" dist-dir)
           args)))

(define (main args)
  (map exec
       (list
        (git "fetch" "--tags" "origin" "develop")
        (git "rebase" "origin/develop" "develop")
        (git "rebase" "develop" "cycle"))))
