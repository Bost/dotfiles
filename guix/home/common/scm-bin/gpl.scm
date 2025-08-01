(define-module (scm-bin gpl)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1) ;; find
  #;(loops for-loops)
  ;; (srfi srfi-42)
  #:use-module (utils) ;; if-let string-in?
  #:export (main))

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gpl) -s
!#

cd $dotf
./guix/home/common/scm-bin/gpl.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define remote-repo-names (list "origin" "github" "gitlab" "codeberg"))

;; TODO define constants & look at modules (git) (guix git) (guix tests git)
;; "git"
;; "rebase"
;; "remote"
;; "fetch"
;; "--tags"
;; "heroku"
;; "origin"
;; "github"
;; "gitlab"
;; "codeberg"
;; "status"
;; "--short"
;; "--branch"
;; "--verbose"
;; "--force"
;; "checkout"
;; "master"
;; "clone"
;; "chmod"

;; TODO unless remote/origin present, try to use remote/upstream or else
;; remote/gitlab or else remote/github

#|
(define (test values)
  (call/cc
   (lambda (continuation-fun)
     (map (lambda (e) (if (eq? e 2) (continuation-fun #t) #f))
          values))))
(test '(1 2 3)) ;; = #t
(test '(1 3 3)) ;; = (#f #f #f)
|#

(define (display-success-msg _)
  ;; (format #t (str "\n# OS command succeeded."
  ;;                 " Avoiding further command calls by"
  ;;                 " breaking out using call/cc..."))
  *unspecified*)

(define (display-failed-msg cmd)
  (format #t "OS command failed:\n~a\n" (string-join cmd " ")))

(define (rebase-bottom-call/cc rebase-args branches)
  "Breakout implementation using call-with-current-continuation"
  (call/cc
   (lambda (continuation-fun)
     (map (lambda (remote)
            (if-let [(r (string-in? remote-repo-names remote))]
              (let* ((cmd (list "git" "fetch" "--tags" r))
                     (ret (exec cmd)))
                (if (= 0 (car ret))
                    (let* ((cmd (append (list "git" "rebase")
                                        (cdr rebase-args)))
                           (ret (exec cmd)))
                      (if (= 0 (car ret))
                          (begin
                            (display-success-msg cmd)
                            #;(continuation-fun #t))
                          (begin
                            (display-failed-msg cmd)
                            (cdr ret))))
                    (begin
                      (display-failed-msg cmd)
                      (cdr ret))))))
          branches))))

(define (rebase-bottom-flag-variable rebase-args branches)
  "Breakout implementation using a flag variable"
  (let ((is-found #f))
    ((comp
      (partial
       map
       (lambda (remote)
         (if is-found
             (format #t "# is-found: ~a; Do nothing.\n" is-found)
             (begin
               (format #t "# is-found: ~a; Calling OS command ...\n" is-found)
               (if-let [(r (string-in? remote-repo-names remote))]
                 (let* ((cmd (list "git" "fetch" "--tags" r))
                        (ret (exec cmd)))
                   (if (= 0 (car ret))
                       (let* ((cmd (append (list "git" "rebase")
                                           (cdr rebase-args)))
                              (ret (exec cmd)))
                         ;; 1st elem of the ret-list is `exec' exit-code
                         (if (= 0 (car ret))
                             (begin
                               (display-success-msg cmd)
                               #;(set! is-found #t))
                             (begin
                               (display-failed-msg cmd)
                               (cdr ret))))
                       (begin
                         (display-failed-msg cmd)
                         (cdr ret))))))
             ))))
     branches)))

(define (main args)
  (when (not (null? (cdr args)))
    (format #t "# Note: 'args' is applied only to `git rebase'\n"))
  ((comp
    (partial rebase-bottom-call/cc args)
    ;; (partial rebase-bottom-flag-variable args)
    ;; dbg
    (partial filter (lambda (remote) (not (string-match "heroku" remote))))
    cdr
    exec)
   (list
    "git" "remote")))

(module-evaluated)
