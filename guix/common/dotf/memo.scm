(define-module (dotf memo)
  #:use-module (bost common utils)
  #:use-module (dotf settings)

  #:use-module (guix memoization)
  #:use-module (ice-9 popen) ; open-input-pipe

;;; (ice-9 readline) requires `guix install guile-readline'. However read-line
;;; might be already in the (ice-9 popen)
  ;; #:use-module (ice-9 readline)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex) ; string-match
  #:use-module (srfi srfi-1) ; delete-duplicates
  ;; #:use-module (guix build utils) ;; invoke - not needed
  #:use-module (ice-9 pretty-print)
)

(define m (module-name-for-logging))
(evaluating-module)

;;; Use function-implementations so that the code below is not evaluated every
;;; time some of the scm-bin CLI utility requiring this module is executed.
(def (hostname)
  (let* [(cmd-result-struct (exec "hostname" #:return-plist #t))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        (let [(result
               (car (plist-get cmd-result-struct #:results)))]
          (format #t "~a ~a hostname is : ~a\n" msgdbg f result)
          result)
        (begin
          (error (format #f "~a ~a retcode: ~a\n" msgerr f retcode)) ; error-out
          ;; (error-command-failed f)
          ;; or return `retcode' instead of `*unspecified*'
          ;; *unspecified*
          ))))
(testsymb 'hostname)

(define-public hostname-memoized (memoize hostname))

(define-public (host-kalus?) (equal? (hostname-memoized) host-kalus))

(define-public (host-ecke?) (equal? (hostname-memoized) host-ecke))

(define-public (host-edge?) (equal? (hostname-memoized) host-edge))

(define-public (host-geek?) (equal? (hostname-memoized) host-geek))

(module-evaluated)
