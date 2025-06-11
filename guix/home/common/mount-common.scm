(define-module (mount-common)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (utils)
  #:export (
            mount unmount eject
            handle-cli)
  )

#|
udisksctl mount --block-device=(lsblk --output PATH,LABEL | rg axagon | awk '{print $1}')
udisksctl mount --block-device=(lsblk --output PATH,LABEL | rg toshiba | awk '{print $1}')

;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (emacs-common) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

cd $dotf && ./guix/home/common/scm-bin/mount-usb.scm axa
cd $dotf && ./guix/home/common/scm-bin/mount-usb.scm toshiba

|#

(define m (module-name-for-logging))
(evaluating-module)

(define utility-name (last (module-name (current-module))))

(define* (get-block-device device-label)
  ;; (format #t "args: ~a\n" args)
  ;; (format #t "device-label: ~a\n" device-label)
  (let* ((command (list
                   "lsblk --output PATH,LABEL | rg" device-label "| awk '{print $1}'"))
         (ret (exec command))
         (retval (car ret)))
    (if (= 0 retval)
        (let* [(output (cdr ret))]
          (if (empty? output)
              (begin
                ;; udisksctl executed w/o specified block-device reports
                ;;   Error looking up object for device
                ;; and returns the retcode 1
                (format #t "No matching device-label found: ~a\n" device-label)
                *unspecified*)
              (car output)))
        (begin
          ;; (error-command-failed "[module]" "extra_info")
          ;; or return `retval' instead of `*unspecified*'
          *unspecified*))))

(define* (mount #:key device-label #:allow-other-keys)
  (let* [(cmd (str "udisksctl mount --block-device="
                   (get-block-device device-label)))]
    ;; (format #t "cmd:\n~a\n" cmd)
    (exec-system* cmd)))

(define* (unmount #:key device-label #:allow-other-keys)
  (let* [(cmd (str "udisksctl unmount --block-device="
                   (get-block-device device-label)))]
    ;; (format #t "cmd:\n~a\n" cmd)
    (exec-system* cmd)))

(define* (eject #:key device-label #:allow-other-keys)
  (let* [(cmd (str "udisksctl power-off --block-device="
                   (get-block-device device-label)))]
    ;; (format #t "cmd:\n~a\n" cmd)
    (exec-system* cmd)))

(define* (handle-cli #:key (verbose #f) utility-name fun device-label #:rest args)
  "All the options, except rest-args, must be specified for the option-spec so
 that the options-parser doesn't complain about e.g. 'no such option: -p'."
  (let* [(f "[handle-cli]")]
    (when verbose
      (format #t "~a ~a utility-name : ~a\n" m f utility-name)
      (format #t "~a ~a fun          : ~a\n" m f fun)
      (format #t "~a ~a device-label : ~a\n" m f device-label)
      (format #t "~a ~a args         : ~a\n" m f args))
    (let* [(elements (list #:verbose #:utility-name #:fun #:device-label))
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
        (apply (partial fun
                        #:verbose verbose
                        #:utility-name utility-name
                        #:gx-dry-run val-gx-dry-run
                        #:device-label device-label)
               val-rest-args)]))))

(module-evaluated)
