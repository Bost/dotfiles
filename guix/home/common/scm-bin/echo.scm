(define-module (scm-bin echo)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)

  ;; TODO seems like it must be added to (srvc scheme-files)
  ;; #:use-module (guix monads)

  #:export (main echo))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ echo) -s
!#

cd $dotf
./guix/home/common/scm-bin/echo.scm -e "'Top\\nBottom'"

|#

(define m (module-name-for-logging))
(evaluating-module)

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
  ((comp
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

(define-inlinable (pipe-bind mv f)
  (let* ((mv-retcode (car mv)))
    (if (= 0 mv-retcode)
        ;; the f-function parses the output
        (f (cadr mv))
        (begin
          (error-command-failed m)
          mv))))

;; (define-monad compose-shell-commands
;;   (bind pipe-bind)
;;   (return pipe-return))

;; (with-monad compose-shell-commands
;;   (>>= (return "uname -o")
;;        exec
;;        (partial echo #:string)
;;        ))

;; (define x "aaa")
;; (define mv (return x))
;; (define f (partial echo #:string))
;; (define g (partial echo #:string))
;; (proper-monad? mv x f g)

(module-evaluated)
