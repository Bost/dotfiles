(define-module (lf)
;;; All used modules must be present in the:
;;;   home-configuration.scm -> service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (ls)
  #:export (main lf))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (lf) -s
!#

|#

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
    ((compose
      (partial apply (partial ls "--list-dirs" "--oneline"))
      (partial map (partial str pwd "/"))
      #;(lambda (args)
        (format #t "(null? args) ~a\n" (null? args))
        (if (null? args) (list "{*,.*}") args)))
     args)))

(define* (main #:rest args)
  "Example:
(main \"<ignored>\" \"-d\" \"lf.scm\")"
  ((compose
    (partial apply lf)
    (partial apply cdr)
    #;dbg)
   args))
