(define-module (scm-bin emacs-editable-profiles)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (guix monads)             #| with-monad   |#
  #:use-module (utils)                   #| partial      |#
  #:use-module (settings)                #| user         |#
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ emacs-editable-profiles) -s
!#

cd $dotf
./guix/home/common/scm-bin/emacs-editable-profiles.scm --version
./guix/home/common/scm-bin/emacs-editable-profiles.scm --gx-dry-run
./guix/home/common/scm-bin/emacs-editable-profiles.scm --version --gx-dry-run
./guix/home/common/scm-bin/emacs-editable-profiles.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define utility-name (last (module-name (current-module))))

(define* (set-editable #:key (verbose #f) utility-name gx-dry-run #:rest args)
  "
Usage:
(set-editable #:gx-dry-run #t \"rest\" \"args\")
(set-editable                 \"rest\" \"args\")
(set-editable                 \"rest\" \"args\")
"
  (let* [(f "[set-config-editable]")
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         (monad (if gx-dry-run
                    compose-commands-guix-shell-dry-run
                    compose-commands-guix-shell))]
    (if gx-dry-run
        (begin
          (format #t "~a ~a monad: ~a\n" m f monad)
          (format #t "~a ~a TODO implement --gx-dry-run\n" m f))
        (let* [(file ".emacs-profiles.el")
               (dst (str home "/" file))
               (src (str (getenv "dotf") "/" file))]
          (when verbose
            (format #t "~a ~a src: ~a\n" m f src)
            (format #t "~a ~a dst: ~a\n" m f dst))
          (with-monad monad
            (>>=
             (return (list dst))
             mdelete-file
             `(override-mv ,src ,dst)
             mcopy-file))
          (copy-file src dst)))))
(testsymb 'set-editable)

(define* (handle-cli #:key (verbose #f) utility-name fun #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'."
  ;; (format #t "~a args: ~a\n" m args)
  (let* [(f "[handle-cli]")
         (args (remove-kw-from-args #:dbg args))
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:fun args))
         (args (remove-kw-from-args #:profile args))
         (args (car args))

          ;; (value #t): a given option expects accept a value
         (option-spec `[
                        (help       (single-char #\h) (value #f))
                        (version    (single-char #\v) (value #f))
                        (gx-dry-run (single-char #\d) (value #f))
                        (rest-args                    (value #f))])

         ;; TODO isn't the #:stop-at-first-non-option swapped?
         (options (getopt-long args option-spec #:stop-at-first-non-option #t))
         ;; #f means that the expected value wasn't specified
         (val-help       (option-ref options 'help       #f))
         (val-version    (option-ref options 'version    #f))
         (val-gx-dry-run (option-ref options 'gx-dry-run #f))
         (val-rest-args  (option-ref options '()         #f))
         ]
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
      (apply (partial fun
                      #:utility-name utility-name
                      #:gx-dry-run val-gx-dry-run)
             val-rest-args)])))
(testsymb 'handle-cli)

(define (main args)
  "Usage:
(main (list \"<ignored>\"))
(main (list \"<ignored>\" \"--help\" \"args\"))
(main (list \"<ignored>\" \"rest\" \"args\"))
(main (list \"<ignored>\" \"--gx-dry-run\" \"rest\" \"args\"))
"
  (handle-cli #:utility-name utility-name
              #:fun set-editable
              args))
(testsymb 'main)

(module-evaluated)
