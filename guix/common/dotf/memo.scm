(define-module (dotf memo)
  #:use-module (dotf utils)
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
(define (hostname)
  (define f (format #f "~a [hostname]" m))
  (let* [(cmd-result-struct (exec "hostname" #:return-plist #t))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        (car (plist-get cmd-result-struct #:results))
        (begin
          (error (format #f "~a retcode: ~a\n" f retcode)) ; error-out
          ;; (error-command-failed f)
          ;; or return `retcode' instead of `*unspecified*'
          ;; *unspecified*
          ))))
(testsymb 'hostname)
;; (format #t "D ~a hostname: ~a\n" m hostname)

(define-public hostname-memoized (memoize hostname))
(testsymb 'hostname-memoized)

;; TODO rename 'is-system-...' to 'is-host-...'

(define-public (is-system-lukas) (equal? (hostname-memoized) host-lukas))
(testsymb 'is-system-lukas)
;; (format #t "D ~a is-system-lukas: ~a\n" m (is-system-lukas))

(define-public (is-system-ecke) (equal? (hostname-memoized) host-ecke))
(testsymb 'is-system-ecke)
;; (format #t "D ~a is-system-ecke: ~a\n" m (is-system-ecke))

(define-public (is-system-edge) (equal? (hostname-memoized) host-edge))
(testsymb 'is-system-edge)
;; (format #t "D ~a is-system-edge: ~a\n" m (is-system-edge))

(define-public (is-system-geek) (equal? (hostname-memoized) host-geek))
(testsymb 'is-system-geek)
;; (format #t "D ~a is-system-geek: ~a\n" m (is-system-geek))

(module-evaluated)
