(define-module (mount-common)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (ice-9 getopt-long) ; command-line arguments handling
  #:use-module (ice-9 regex)       ; string-match
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (dotf utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)     ; define*-public
  #:use-module (srfi srfi-26)      ; special selected function parameters
  )

#|
udisksctl mount --block-device=(lsblk --output PATH,LABEL | rg axagon | awk '{print $1}')
udisksctl mount --block-device=(lsblk --output PATH,LABEL | rg toshiba | awk '{print $1}')

;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (mount-common) -s
!#

cd $dotf && ./guix/home/common/scm-bin/mount-usb.scm axa
cd $dotf && ./guix/home/common/scm-bin/mount-usb.scm toshiba

This module is not directly executed. No main-procedure is needed.
|#

(define m (module-name-for-logging))
(evaluating-module)

(define*-public (udisksctl command device-name-pattern)
  "Example:
(udisksctl \"info\" \"axa\")
(udisksctl \"info\" \"crucial\")
(udisksctl \"mount\" \"crucial\")
(udisksctl \"info\" \"xxx\") ;=> error
(udisksctl \"mount\" \"xxx\") ;=> error"
  ((comp
    (lambda (cmd-result-struct)
      (match cmd-result-struct ; match is a macro
        [(#:retcode retcode #:results results)
         (cond
          [(not (zero? retcode)) (exit retcode)]
          [else (for-each (partial format #t "~a\n") results)])]))
    (cut exec <> #:return-plist #t)
    ;; udisksctl executed w/o specified device-label reports
    ;;   Error looking up object for device
    ;; and returns the retcode 1
    (partial format #f "udisksctl ~a --block-device=~a" command)
    (lambda (cmd-result-struct)
      (match cmd-result-struct ; match is a macro
        [(#:retcode retcode #:results results)
         (cond
          [(not (zero? retcode)) (exit retcode)]
          [(empty? results)
           ((comp
             (lambda (_) (exit 1)) ; retcode of `rg' not matching anything
             ;; (error-command-failed "[module]" "extra_info")
             (partial error-command-failed m)
             (partial format #f "No matching device found: ~a"))
            device-name-pattern)]
          [else (car results)])]))
    (cut exec <> #:return-plist #t)
    (partial format #f "lsblk --output PATH,LABEL | rg ~a | awk '{print $1}'"))
   device-name-pattern))

(define*-public (mount #:key params #:allow-other-keys)
  "Example:
(mount #:params \"axa\")"
  (udisksctl "mount" params))

(define*-public (unmount #:key params #:allow-other-keys)
  "Example:
(unmount #:params \"axa\")"
  (udisksctl "unmount" params))

(define*-public (eject #:key params #:allow-other-keys)
  "Example:
(eject #:params \"axa\")"
  (udisksctl "power-off" params))

(define*-public (info #:key params #:allow-other-keys)
  "Example:
(info #:params \"axa\")"
  (udisksctl "info" params))

(module-evaluated)
