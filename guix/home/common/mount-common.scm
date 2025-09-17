(define-module (mount-common)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; last
  #:use-module (utils)
  #:export (udisksctl mount unmount eject))

#|
udisksctl mount --block-device=(lsblk --output PATH,LABEL | rg axagon | awk '{print $1}')
udisksctl mount --block-device=(lsblk --output PATH,LABEL | rg toshiba | awk '{print $1}')

;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (mount-common) -s
!#

This file is not meant to be executed directly, thus no main functions is
defined.

cd $dotf && ./guix/home/common/scm-bin/mount-usb.scm axa
cd $dotf && ./guix/home/common/scm-bin/mount-usb.scm toshiba

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (udisksctl command device-name-pattern)
  ;; (format #t "device-name-pattern : ~a\n" device-name-pattern)
  (call/cc
   (lambda (exit)
     ((comp
       exec-system*
       ;; udisksctl executed w/o specified device-lable reports
       ;;   Error looking up object for device
       ;; and returns the retcode 1
       (partial format #f "udisksctl ~a --block-device=~a" command)
       (lambda (ret)
         (let* [(retval (car ret))]
           (if (= 0 retval)
               (let* [(output (cdr ret))]
                 (if (empty? output)
                     ((comp
                       exit
                       ;; (error-command-failed "[module]" "extra_info")
                       (partial error-command-failed m)
                       (partial format #f "No matching device found: ~a"))
                      device-name-pattern)
                     (car output)))
               ;; return `retval' or `*unspecified*'
               (exit retval))))
       exec
       (partial format #f
                "lsblk --output PATH,LABEL | rg ~a | awk '{print $1}'"))
      device-name-pattern))))

(define* (mount #:key params #:allow-other-keys)
  (udisksctl "mount" params))

(define* (unmount #:key params #:allow-other-keys)
  (udisksctl "unmount" params))

(define* (eject #:key params #:allow-other-keys)
  (udisksctl "power-off" params))

(module-evaluated)
