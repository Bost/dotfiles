(define-module (emacs-pkill)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  ;; #:use-module (guix monads)
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (handle-cli pkill-server))

(define m (module-name-for-logging))
(evaluating-module)

(define dbg #f)

;; TODO the value of (which-emacs) may or may not be a closure, with it's value
;; closed at the runtime of `gxhre`
;; Code dupllication, see the emacs-config-launcher module
(define-public (which-emacs)
  "(which-emacs) => \"/home/bost/.guix-home/profile/bin/emacs\""
  ;; (emacs-binary-path)
  ((@(guix build utils) which) "emacs"))

(define* (pkill-server #:key gx-dry-run profile #:rest args)
  "
Usage:
(pkill-server #:gx-dry-run #t #:profile \"spacemacs\" \"rest\" \"args\")
(pkill-server #:gx-dry-run #t #:profile \"spguimacs\" \"rest\" \"args\")
(pkill-server                 #:profile \"spacemacs\" \"rest\" \"args\")
(pkill-server                 #:profile \"spguimacs\" \"rest\" \"args\")
"
  (let* [
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         ]
    ;; (format #t "~a gx-dry-run : ~a\n" m gx-dry-run)
    ;; (format #t "~a profile : ~a\n" m profile)
    ;; (format #t "~a args : ~a\n" m args)
    ;; pkill-pattern must NOT be eclosed by \"\"
    (let* [(pkill-pattern
            (format #f "~a --with-profile=~a --daemon"
                    (which-emacs) profile))]
      ;; TODO use with-monad
      (apply exec-system*-new
             #:split-whitespace #f
             #:gx-dry-run gx-dry-run
             "pkill" "--full" pkill-pattern args))))
(testsymb 'pkill-server)

(define* (handle-cli #:key utility-name fun #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so that
the options-parser doesn't complain about e.g. 'no such option: -p'."
  ;; (when dbg (format #t "~a args: ~a\n" m args))
  (let* [
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:fun args))
         (args (car args))
         ]
    (let* [(option-spec
            '[
              (help       (single-char #\h))
              #;(version    (single-char #\v))
              (gx-dry-run (single-char #\d))
              ])]
      (when dbg
        (format #t "~a args        : ~a\n" m args)
        (format #t "~a option-spec : ~a\n" m option-spec))
      ;; TODO isn't the #:stop-at-first-non-option swapped?
      (let* [(options (getopt-long args option-spec #:stop-at-first-non-option #t))
             ;; #f means that the expected value wasn't specified
             (val-help       (option-ref options 'help       #f))
             #;(val-version    (option-ref options 'version    #f))
             (val-gx-dry-run (option-ref options 'gx-dry-run #f))
             (val-rest-args  (option-ref options '()         #f))
             ]
        (when dbg
          (format #t "~a options        : ~a\n" m options)
          (format #t "~a val-help       : ~a\n" m val-help)
          #;(format #t "~a val-version    : ~a\n" m val-version)
          (format #t "~a val-gx-dry-run : ~a\n" m val-gx-dry-run)
          (format #t "~a val-rest-args  : ~a\n" m val-rest-args))
        (cond
         [val-help
          (format #t "~a [options] ~a\n~a\n\n"
                  utility-name
                  #;"    -v, --version    Display version"
                  "    -h, --help       Display this help")]
         #;[val-version
          (format #t "~a version 1.23
" utility-name)]
         [#t
          (fun args)])))))
(testsymb 'handle-cli)

(module-evaluated)
