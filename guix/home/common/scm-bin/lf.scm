(define-module (scm-bin lf)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (scm-bin ls)
  #:use-module (utils)
  #:export (main lf))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ lf) -s
!#

cd $dotf
./guix/home/common/scm-bin/lf.scm .envrc

|#

(evaluating-module)

;;; TODO get the content of the current working directory, i.e emulate the
;;; globing expansion of "{*,.*}"
(define* (lf #:rest args)
  "Examples:
;; See (getenv \"PWD\") when 'No such file or directory (os error 2)'
(lf \"lf.scm\" \"ls.scm\")
(lf \"lf.scm ls.scm\")
(equal? (lf \"lf.scm\" \"ls.scm\")
        (lf \"lf.scm ls.scm\"))
;; > #t

(lf \".spguimacs\" \".spacemacs\")
(lf \".spguimacs\" \".spacemacs\")
(equal? (lf \".spguimacs\" \".spacemacs\")
        (lf \".spguimacs\" \".spacemacs\"))
;; > #t
"
  (let* [(pwd (getenv "PWD"))]
    ((comp
      (partial apply (partial ls "--list-dirs" "--oneline"))
      (partial map (partial str pwd "/"))
      #;(lambda (args)
        (format #t "(null? args) ~a\n" (null? args))
        (if (null? args) (list "{*,.*}") args)))
     args)))

(define* (main #:rest args)
  "Example:
(main \"<ignored>\" \"-d\" \"lf.scm\")"
  ((comp
    (partial apply lf)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
