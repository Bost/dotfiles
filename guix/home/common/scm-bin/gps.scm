(define-module (scm-bin gps)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (ice-9 popen)
  #:use-module (utils)
  #:use-module (scm-bin git-remote)
  #:export (main gps gps-all))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gps) -s
!#

cd $dotf
./guix/home/common/scm-bin/gps.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (gps #:key remote #:allow-other-keys #:rest args)
  "git-push"
  (let* [(elements (list #:remote))
         (args (remove-all-elements args elements))
         (ret (exec (append
                     (list "git" "push")
                     args
                     (list remote)
                     )))]
    ;; (format #t "args : ~a\n" args)
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          ;; Process output.
          ;; if `git push ...` needs to return anything more except just a
          ;; retcode - implement it here
          (map (partial format #t "~a\n") output)
          ret)
        (begin
          (error-command-failed m)
          *unspecified*))))
(testsymb 'gps)

(define* (gps-all #:rest args)
  "This git-pushes to all remote repos. TODO implement basic `gps`"
  ((comp
    (lambda (ret-vals) ;; the reducer
      ;; ret-vals is a list consisting of sublists:
      ;; ((ret-code-0 list-of-vals-0)
      ;;  (ret-code-N list-of-vals-N))
      ;; where ret-code-i is a number, list-of-vals-i can consist of any values
      (list (apply max (map car ret-vals)) (map cdr ret-vals)))

    (partial map
             (comp
              (lambda (remote)
                (apply (partial gps #:remote remote "--follow-tags"
                                ;; "--verbose"
                                )
                       args))
              car))
    (partial filter (lambda (remote-url)
                      (not (null? (cdr remote-url)))))
    (partial map
             (lambda (remote)
               (cons remote
                     ((comp
                       (partial filter
                                (lambda (url)
                                  (string-match "git@" url)))
                       cdr
                       (partial git-remote "get-url"))
                      remote))))
    (partial filter (lambda (remote)
                      (not (string-match "heroku" remote))))
    cdr)
   (git-remote)))
(testsymb 'gps-all)

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"arg0\")"
  ((comp
    (partial apply gps-all)
    ;; (partial apply gps #:remote "github")
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
