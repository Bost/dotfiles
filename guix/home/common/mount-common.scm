(define-module (mount-common)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; last
  #:use-module (utils)
  #:export (mount unmount eject))

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

(module-evaluated)
