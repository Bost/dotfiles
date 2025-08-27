(define-module (scm-bin chmod)
;;; All used modules must be present in the module (services scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main chmod))

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ chmod) -s
!#

touch /tmp/foo /tmp/fox
cd $dotf
./guix/home/common/scm-bin/chmod.scm rw /tmp/foo /tmp/fox
ls -la /tmp/foo /tmp/fox

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (chmod #:rest args)
  "Usage:
(chmod (list \"<ignored>\" \"rw\" \"/tmp/foo\" \"/tmp/fox\"))
"
  ;; (format #t "args: '~a'\n" args)
  (let* [(arg-lst (car args))
         (modifier ((comp car cdr) arg-lst))]
    ;; (format #t "arg-lst: '~a'\n" arg-lst)
    ;; (format #t "modifier: '~a'\n" modifier)
    (let* [(files ((comp cdr cdr) arg-lst))]
      ;; (format #t "files: '~a'\n" files)
      (apply exec-system*
             "chmod" (string-append "+" modifier)
             files))))

(define main chmod)

(testsymb 'main)

(module-evaluated)
