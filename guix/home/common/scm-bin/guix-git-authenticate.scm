(define-module (scm-bin guix-git-authenticate)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (fs-utils)  ; dgx (repository location)
  #:use-module (settings)  ; home
  #:export (main guix-git-authenticate)
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-git-authenticate) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-authenticate.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

;; (define repo dgx)
;; (define signer "\"5D02 7CEF 97C8 FE6D E393  70BD 9403 F3A0 D4BA FE08\"")
;; (define cache-key "channels/guix")
;; (define first-commit "2e1ead7c8b")
;; (define last-commit "master") ; i.e. branch-name

(define repo (str home "/dev/guix-rust-past-crates"))
(define signer (getenv "gpgPubKey"))
(define cache-key "channels/guix-rust-past-crates")
;; last previously authenticated commit
(define first-commit "6fdba3fbf3a62aa53b3e5d1c57a7bde5727f986c")
(define last-commit "trunk") ; commit or branch-name

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
          (let* [(commits (reverse output))]
            (format #t "~a commits to authenticate:\n" (length commits))
            (map (partial format #t "~a\n") commits)
            (format #t "\n")
            commits))
        (begin
          (error-command-failed m)
          *unspecified*))))
(testsymb 'get-commits)

(define* (authenticate-commit #:key repo commit signer)
  "Examples:
(authenticate-commit #:repo repo #:signer signer #:commit)
"
  (let* [(cmd
          (list
           "guix" "git" "authenticate"
           ;; --cache-key=path/to/KEY reads ~/.cache/guix/authentication/path/to/KEY
           (format #f "--cache-key=~a" cache-key)
           "--stats"
           (format #f "--repository=~a ~a ~a"
                   repo commit signer)))
         (ret (exec cmd))]
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
(define* (guix-git-authenticate #:rest args)
  "Examples:
(guix-git-authenticate \"-f\" \"arg0\")
(guix-git-authenticate \"-f arg0\")
(equal? (guix-git-authenticate \"-f\" \"arg0\")
        (guix-git-authenticate \"-f arg0\"))
;; > #t
"
  (let* [(elements (list #:remote))
         (args (remove-all-elements args elements))
         (commits (list first-commit)
          ;; (get-commits
          ;;  #:repo repo
          ;;  #:beg first-commit
          ;;  #:end last-commit)
          )]
    (map (partial authenticate-commit #:repo repo #:signer signer #:commit)
                  commits)))
(testsymb 'guix-git-authenticate)

(define* (main #:rest args)
  "Examples:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply guix-git-authenticate)
    (partial apply cdr)
    dbg)
   args))
(testsymb 'main)

(module-evaluated)
