(define-module (command-line)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (guix monads)       ; with-monad
  #:use-module (utils)             ; partial
  #:use-module (settings)          ; user
  #:use-module (ice-9 exceptions)
  #:export (handle-cli))

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (command-line) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

|#

(define m (module-name-for-logging))
(evaluating-module)

(define utility-name (last (module-name (current-module))))

(define mount-procedures '(pkill-server create-launcher set-editable))
(define emacs-procedures '(mount unmount eject))

(define-exception-type &handle-cli-exception &exception make-handle-cli-exception handle-cli-exception?
  ;; (field-name field-accessor) ...
  (handle-cli-procedure handle-cli-exception-procedure))

(define* (handle-cli
          #:key (verbose #t) utility-name fun profile device-label
          ;; #:allow-other-keys
          #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'."
  (let* [(f "[handle-cli]")]
    (when verbose
      (format #t "~a ~a utility-name : ~a\n" m f utility-name)
      (format #t "~a ~a fun          : ~a\n" m f fun)
      (format #t "~a ~a profile      : ~a\n" m f profile)
      (format #t "~a ~a device-label : ~a\n" m f device-label)
      (format #t "~a ~a args         : ~a\n" m f args))
    (let* [(elements (list #:verbose #:utility-name #:fun
                           #:profile #:device-label))
           (args (remove-all-elements args elements))
           (args (car args))

           ;; (value #t): a given option expects accept a value
           (option-spec `[(help       (single-char #\h) (value #f))
                          (version    (single-char #\v) (value #f))
                          (gx-dry-run (single-char #\d) (value #f))
                          (rest-args                    (value #f))])

           ;; TODO isn't the #:stop-at-first-non-option swapped?
           (options (getopt-long args option-spec #:stop-at-first-non-option #t))
           ;; #f means that the expected value wasn't specified
           (val-help       (option-ref options 'help       #f))
           (val-version    (option-ref options 'version    #f))
           (val-gx-dry-run (option-ref options 'gx-dry-run #f))
           (val-rest-args  (option-ref options '()         #f))]
      (when verbose
        (format #t "~a ~a option-spec    : ~a\n" m f option-spec)
        (format #t "~a ~a options        : ~a\n" m f options)
        (format #t "~a ~a val-help       : ~a\n" m f val-help)
        (format #t "~a ~a val-version    : ~a\n" m f val-version)
        (format #t "~a ~a val-gx-dry-run : ~a\n" m f val-gx-dry-run)
        (format #t "~a ~a val-rest-args  : ~a\n" m f val-rest-args))
      (cond
       [val-help
        (format #t "~a [options]\n~a\n~a\n\n"
                utility-name
                "    -v, --version    Display version"
                "    -h, --help       Display this help")]
       [val-version
        (format #t "~a version <...>\n" utility-name)]
       [#t
        (let* [(prms
                (cond
                 [(member? fun emacs-procedures)
                  (list #:device-label device-label)]
                 [(member? fun mount-procedures)
                  (list #:profile profile)]
                 [#t
                  (raise-exception
                   (make-exception
                    (make-handle-cli-exception fun)
                    (make-exception-with-message
                     (format #t "The procedure must be one of:\n  ~a\n\n"
                             (append emacs-procedures mount-procedures)))))]))]

          (apply (eval fun (current-module)) ;; resolve symbol to the procedure
                 (append (list #:utility-name utility-name
                               #:gx-dry-run   val-gx-dry-run
                               #:verbose      verbose)
                         prms val-rest-args)))]))))
(testsymb 'handle-cli)

(module-evaluated)

