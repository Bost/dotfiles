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
;; -e calls the `main` function

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gpl) -s
!#

cd $dotf
./guix/home/common/scm-bin/gpl.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define origin-remotes (list "origin" "github" "gitlab"))

;; TODO if remote/origin is not present then try to use remote/upstream, then
;; remote/gitlab, then remote/github

#|
(define (test values)
  (call/cc
   (lambda (continuation-fun)
     (map (lambda (e) (if (eq? e 2) (continuation-fun #t) #f))
          values))))
(test '(1 2 3)) ;; = #t
(test '(1 3 3)) ;; = (#f #f #f)
|#

(define (rebase-bottom-call/cc rebase-args branches)
  "Breakout implementation using call-with-current-continuation"
  (call/cc
   (lambda (continuation-fun)
     (map (lambda (remote)
            (if-let [(r (string-in? origin-remotes remote))]
              (let* ((cmd (list "git" "fetch" "--tags" r))
                     (ret (exec cmd)))
                (if (= 0 (car ret))
                    (let* ((cmd (append (list "git" "rebase")
                                        (cdr rebase-args)))
                           (ret (exec cmd)))
                      (if (= 0 (car ret))
                          (begin
                            (format
                             #t
                             (str "\n# OS command succeeded."
                                  " Avoiding further command calls by"
                                  " breaking out using call/cc..."))
                            (continuation-fun #t))
                          (begin
                            #;(format #t "Command failed:\n~a\n"
                            (string-join cmd " "))
                            (cdr ret))))
                    (begin
                      #;(format #t "Command failed:\n~a\n"
                      (string-join cmd " "))
                      (cdr ret))))))
          branches))))

(define (rebase-bottom-flag-variable rebase-args branches)
  "Breakout implementation using a flag variable"
  (let ((found #f))
    ((comp
      (partial
       map
       (lambda (remote)
         (if found
             (format #t "# found is: ~a; Do nothing.\n" found)
             (begin
               (format #t "# found is: ~a; Calling OS command ...\n" found)
               (if-let [(r (string-in? origin-remotes remote))]
                 (let* ((cmd (list "git" "fetch" "--tags" r))
                        (ret (exec cmd)))
                   (if (= 0 (car ret))
                       (let* ((cmd (append (list "git" "rebase")
                                           (cdr rebase-args)))
                              (ret (exec cmd)))
                         ;; 1st elem of the ret-list is `exec' exit-code
                         (if (= 0 (car ret))
                             (begin
                               (format
                                #t
                                (str "\n# OS command succeeded."
                                     " Avoiding further command calls by"
                                     " setting & checking a flag..."))
                               (set! found #t))
                             (begin
                               #;(format #t "Command failed:\n~a\n"
                               (string-join cmd " "))
                               (cdr ret))))
                       (begin
                         #;(format #t "Command failed:\n~a\n"
                         (string-join cmd " "))
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
