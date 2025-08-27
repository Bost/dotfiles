(define-module (command-line)
;;; All used modules must be present in the module (services scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)       ; last
  #:use-module (guix monads)       ; with-monad
  #:use-module (utils)             ; partial
  #:use-module (settings)          ; user
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

(define emacs-procedures '(pkill-server create-launcher set-editable))
(define mount-procedures '(mount unmount eject))

(define-exception-type &handle-cli-exception &exception make-handle-cli-exception handle-cli-exception?
  ;; (field-name field-accessor) ...
  (handle-cli-procedure handle-cli-exception-procedure))

(define* (handle-cli
          #:key (verbose #t) program-name fun profile device-label
          ;; #:allow-other-keys
          #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'."
  (define f (format #f "~a [handle-cli]" m))
  (when verbose
    (format #t "~a program-name : ~a\n" f program-name)
    (format #t "~a fun          : ~a\n" f fun)
    (format #t "~a profile      : ~a\n" f profile)
    (format #t "~a device-label : ~a\n" f device-label)
    (format #t "~a args         : ~a\n" f args))
  (let* [(elements (list #:verbose #:program-name #:fun
                         #:profile #:device-label))
         (args (remove-all-elements args elements))
         (args (car args))

         ;; (value #t): a given option expects accept a value
         (option-spec `[(help       (single-char #\h) (value #f))
                        (version    (single-char #\v) (value #f))
                        (gx-dry-run (single-char #\d) (value #f))
                        (create-frame (single-char #\c) (value #f))
                        (rest-args                    (value #f))])

         ;; TODO isn't the #:stop-at-first-non-option swapped?
         (options (getopt-long args option-spec #:stop-at-first-non-option #t))
         ;; #f means that the expected value wasn't specified
         (val-help       (option-ref options 'help       #f))
         (val-version    (option-ref options 'version    #f))
         (val-gx-dry-run (option-ref options 'gx-dry-run #f))
         (val-create-frame (option-ref options 'create-frame #f))
         (val-rest-args  (option-ref options '()         #f))]
    (when verbose
      (format #t "~a option-spec    : ~a\n" f option-spec)
      (format #t "~a options        : ~a\n" f options)
      (format #t "~a val-help       : ~a\n" f val-help)
      (format #t "~a val-version    : ~a\n" f val-version)
      (format #t "~a val-gx-dry-run : ~a\n" f val-gx-dry-run)
      (format #t "~a val-create-frame : ~a\n" f val-create-frame)
      (format #t "~a val-rest-args  : ~a\n" f val-rest-args))
    (cond
     [val-help
      (format #t "~a [options]\n~a\n~a\n\n"
              program-name
              "    -v, --version    Display version"
              "    -h, --help       Display this help")]
     [val-version
      (format #t "~a version <...>\n" program-name)]
     [#t
      (let* [(prms
              (cond
               [(member? fun emacs-procedures)
                (list #:profile profile)]
               [(member? fun mount-procedures)
                (list #:device-label device-label)]
               [#t
                (raise-exception
                 (make-exception
                  (make-handle-cli-exception fun)
                  (make-exception-with-message
                   (format #t "The procedure must be one of:\n  ~a\n\n"
                           (append emacs-procedures mount-procedures)))))]))]
        (apply (eval fun (current-module)) ;; resolve symbol to the procedure
               (append
                (list #:utility-name program-name
                      #:gx-dry-run   val-gx-dry-run
                      #:verbose      verbose)
                (if (equal? fun 'create-launcher)
                    (list #:create-frame val-create-frame)
                    (list))
                prms val-rest-args)))])))
(testsymb 'handle-cli)

(module-evaluated)

;; (begin (use-modules (ice-9 getopt-long) (ice-9 regex) (guix monads) (utils) (settings) (command-line) (emacs-common))
;;        (handle-cli #:verbose #f #:fun (quote create-launcher)
;;                    #:profile "guix"
;;                    (command-line)))
