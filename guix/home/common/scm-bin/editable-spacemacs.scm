(define-module (scm-bin editable-spacemacs)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (editable-emacs-config)
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ editable-spacemacs) -s
!#

cd $dotf
./guix/home/common/scm-bin/editable-spacemacs.scm --gx-dry-run
./guix/home/common/scm-bin/editable-spacemacs.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define dbg #t)
(define utility-name (last (module-name (current-module))))

(define (fun args)
  "All the options, including(!) rest-args, must be specified for the option-spec
so that the options-parser doesn't complain about e.g. 'no such option: -p'."
  (let* [(option-spec
          ;; (value #t): a given option expects accept a value
          `[
            (help       (single-char #\h) (value #f))
            (version    (single-char #\v) (value #f))
            (gx-dry-run (single-char #\d) (value #f))
            (rest-args                    (value #f))
            ])]
    ;; (format #t "~a option-spec : ~a\n" m option-spec)
    (let* [(options (getopt-long args option-spec))
           ;; #f means that the expected value wasn't specified
           (val-gx-dry-run (option-ref options 'gx-dry-run #t))
           (val-rest-args  (option-ref options '()         #f))
           ]
      (when dbg
        (format #t "~a options        : ~a\n" m options)
        (format #t "~a val-gx-dry-run : ~a\n" m val-gx-dry-run)
        (format #t "~a val-rest-args  : ~a\n" m val-rest-args))
      (begin
        (apply
         (partial set-config-editable
                  #:gx-dry-run val-gx-dry-run
                  #:profile spacemacs)
         val-rest-args)))))
(testsymb 'fun)

(define (main args)
  "Usage:
(main (list \"<ignored>\" \"--help\" \"args\"))
(main (list \"<ignored>\" \"rest\" \"args\"))
(main (list \"<ignored>\" \"--gx-dry-run\" \"rest\" \"args\"))
"
  (handle-cli #:utility-name utility-name #:fun fun args))
(testsymb 'main)

(module-evaluated)
