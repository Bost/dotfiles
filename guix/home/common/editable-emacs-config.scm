(define-module (editable-emacs-config)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (guix monads)
  #:use-module (ice-9 getopt-long) ;; command-line arguments handling
  #:export (handle-cli set-config-editable))

(define m (module-name-for-logging))
(evaluating-module)

(define dbg #f)

;; TODO test the gx-dry-run
(define* (set-config-editable #:key gx-dry-run profile #:rest args)
  "
Usage:
(set-config-editable #:gx-dry-run #t #:profile \"spacemacs\" \"rest\" \"args\")
(set-config-editable #:gx-dry-run #t #:profile \"spguimacs\" \"rest\" \"args\")
(set-config-editable                 #:profile \"spacemacs\" \"rest\" \"args\")
(set-config-editable                 #:profile \"spguimacs\" \"rest\" \"args\")
"
  (let* [
         (args (remove-kw-from-args #:gx-dry-run args))
         (args (remove-kw-from-args #:profile args))
         ]
    ;; (format #t "~a profile : ~a\n" m profile)
    ;; (format #t "~a args : ~a\n" m args)
    (let* [(monad (if gx-dry-run
                      compose-commands-guix-shell-dry-run
                      compose-commands-guix-shell))]
      (when gx-dry-run
        (format #t "~a monad: ~a\n" m monad))
      (let* [(dst (format #f "~a/.emacs.d.distros/~a-config/init.el"
                          (getenv "HOME") profile))
             (src (format #f "~a/.~a" (getenv "dotf") profile))]
        (with-monad monad
          (>>=
           (return (list dst))
           mdelete-file
           `(override-mv ,src ,dst)
           mcopy-file))
        (copy-file src dst)))))
(testsymb 'set-config-editable)

;; TODO --version is not needed
(define* (handle-cli #:key utility-name fun #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so that
the options-parser doesn't complain about e.g. 'no such option: -p'."
  ;; (format #t "~a args: ~a\n" m args)
  (let* [
         (args (remove-kw-from-args #:utility-name args))
         (args (remove-kw-from-args #:fun args))
         ]
    (let* [(option-spec
            '[
              (help       (single-char #\h))
              (version    (single-char #\v))
              (gx-dry-run (single-char #\d))
              ])]
      ;; TODO isn't the #:stop-at-first-non-option swapped?
      (let* [(options (getopt-long args option-spec #:stop-at-first-non-option #t))
             ;; #f means that the expected value wasn't specified
             (val-help       (option-ref options 'help    #f))
             (val-version    (option-ref options 'version #f))
             (val-rest-args  (option-ref options '()      #f))]
        (when dbg
          (format #t "~a option-spec   : ~a\n" m option-spec)
          (format #t "~a val-help      : ~a\n" m val-help)
          (format #t "~a val-version   : ~a\n" m val-version)
          (format #t "~a val-rest-args : ~a\n" m val-rest-args))
        (cond
         [(option-ref options 'help #f)
          (format #t "~a [options]
    -v, --version    Display version
    -h, --help       Display this help
" utility-name)]
         [(option-ref options 'version #f)
          (format #t "~a version 1.23
" utility-name)]
         [#t
          (fun args)])))))
(testsymb 'handle-cli)

(module-evaluated)
