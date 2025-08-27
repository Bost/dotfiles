(define-module (scm-bin echo)
;;; All used modules must be present in the module (services scheme-files) under:
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

;; (with-monad compose-shell-commands
;;   (>>=
;;    (return "uname -o")
;;    exec   ; => (0 "GNU/Linux")
;;    (partial echo #:string)))

(module-evaluated)
