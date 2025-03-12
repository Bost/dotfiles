(define-module (scm-bin git-authenticate)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (fs-utils)  ; dgx (repository location)
  #:export (main git-authenticate)
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-authenticate) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-authenticate.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define repo dgx)
(define signer "\"5D02 7CEF 97C8 FE6D E393  70BD 9403 F3A0 D4BA FE08\"")

(define* (get-commits #:key repo beg end)
  "Examples:
(get-commits #:beg \"90f0f8713d\" #:end \"master\")
"
  (let* [(ret (exec
               (append
                (list "git"
                      (format #f "--git-dir=~a/.git" repo)
                      "log" "--pretty=format:%H"
                      (format #f "~a..~a" beg end)))))]
    (if (= 0 (car ret))
        (let* [(output (cdr ret))]
          ;; Process output:
          (let* [(commits output)]
            (format #t "~a commits to authenticate\n" (length commits))
            commits))
        (begin
          (error-command-failed m)
          *unspecified*))))
(testsymb 'get-commits)

(define* (authenticate-commit #:key repo commit signer)
  "Examples:
(authenticate-commit #:repo repo #:signer signer #:commit)
"
  (let* [(ret (exec (append
                     (list "guix" "git" "authenticate"
                           "--cache-key=channels/guix --stats"
                           (format #f "--repository=~a ~a ~a"
                                   repo commit signer)))))]
    (if (= 0 (car ret))
        (let* [(output (cdr ret))]
          ;; Process output:
          ;; if `git push ...` needs to return anything more except just a
          ;; retcode - implement it here
          (map (partial format #t "~a\n") output)
          ret)
        (begin
          (error-command-failed m)
          *unspecified*))))
(testsymb 'authenticate-commit)

;; TODO pass the repo and beg (first commit to authenticate) arguments from CLI
(define* (git-authenticate #:rest init-args)
  "Examples:
(git-authenticate \"-f\" \"arg0\")
(git-authenticate \"-f arg0\")
(equal? (git-authenticate \"-f\" \"arg0\")
        (git-authenticate \"-f arg0\"))
;; > #t
"
  (let* [(args (remove-kw-from-args #:remote init-args))
         (commits (get-commits
                   #:repo repo
                   #:beg "2e1ead7c8b"
                   #:end "master"))]
    (map (partial authenticate-commit #:repo repo #:signer signer #:commit)
                  commits)))
(testsymb 'git-authenticate)

(define* (main #:rest args)
  "Examples:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply git-authenticate)
    (partial apply cdr)
    dbg)
   args))
(testsymb 'main)

(module-evaluated)
