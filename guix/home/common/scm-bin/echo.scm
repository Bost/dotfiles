(define-module (scm-bin echo)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main echo))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ echo) -s
!#

cd $dotf
./guix/home/common/scm-bin/echo.scm -e "'Top\\nBottom'"

|#

(define m (module-name-for-logging))
(evaluating-module m)

(define* (echo #:key (options "") (string ""))
  "
(echo #:options \"-e\" #:string \"Top\\\\nBottom\")
$ echo -e Top\\nBottom
Top
Bottom
;; => (0 \"Top\" \"Bottom\")"
  ;; (format #t "options : ~a\n" options)
  ;; (format #t "string : ~a\n" string)
  (let* ((ret (exec
               (list "echo" options string)
               ;; (append (list "echo") (list options) (list string))
               )))
    (map (partial format #t "~a\n") (cdr ret))
    ret))
(testsymb 'echo)

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"arg0\")"
  ((compose
    (partial apply (lambda (options string)
                     (echo #:options options #:string string)))
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(define-inlinable (pipe-return command)
  (list
   ;; Return code signaling that some hypothetical previous command terminated
   ;; successfully.
   0
   ;; String containing the command to execute as next
   command))

(define-inlinable (pipe-bind m f)
  (let* ((m-retcode (car m)))
    (if (= 0 m-retcode)
        ;; the f-function parses the output
        (f (cadr m))
        (begin
          (format #t "~a\n" (error-command-failed))
          m))))

;; (define-monad compose-shell-commands
;;   (bind pipe-bind)
;;   (return pipe-return))

;; (with-monad compose-shell-commands
;;   (>>= (return "uname -o")
;;        exec
;;        (partial echo #:string)
;;        ))

;; (define x "aaa")
;; (define m (return x))
;; (define f (partial echo #:string))
;; (define g (partial echo #:string))
;; (proper-monad? m x f g)

(module-evaluated m)
