(define-module (scm-bin emacs-launcher-spacemacs)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils) ;; partial
  #:use-module (settings)
  #:use-module (emacs-config-launcher)
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (main))

#|
;; -e calls the `main` function

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ emacs-launcher-spacemacs) -s
!#

cd $dotf
./guix/home/common/scm-bin/emacs-launcher-spacemacs.scm --version

set f $dotf/guix/home/common/scm-bin/emacs-launcher-spacemacs.scm
./guix/home/common/scm-bin/emacs-launcher-spacemacs.scm $f

|#

(define m (module-name-for-logging))
(evaluating-module)

(define dbg #t)
(define utility-name (last (module-name (current-module))))

(define (fun args)
  "All the options, including(!) rest-args, must be specified for the option-spec
so that the options-parser doesn't complain about e.g. 'no such option: -p'."
  (let* [(f "[fun]")
         (option-spec
          ;; (value #t): a given option expects accept a value
          `[
            (help       (single-char #\h) (value #f))
            (version    (single-char #\v) (value #f))
            (rest-args                    (value #f))
            ])]
    ;; (format #t "~a option-spec : ~a\n" m option-spec)
    (let* [(options (getopt-long args option-spec))
           ;; #f means that the expected value wasn't specified
           (val-rest-args  (option-ref options '()         #f))
           ]
      (when dbg
        (format #t "~a ~a options       : ~a\n" m f options)
        (format #t "~a ~a val-rest-args : ~a\n" m f val-rest-args))
      (begin
        (apply
         (partial create-emacs-launcher #:profile spacemacs)
         val-rest-args)))))

(define (main args)
  "Usage:
(main (list \"<ignored>\" \"--help\" \"args\"))
(main (list \"<ignored>\" \"rest\" \"args\"))
"
  (let* [(f "[main]")]
    ;; (format #t "~a ~a args : ~a\n" m f args)
    (handle-cli #:utility-name utility-name #:fun fun args)))
(testsymb 'main)

(module-evaluated)
