(define-module (memo)
  #:use-module (utils)
  #:use-module (settings)

  #:use-module (guix memoization)
  ;; open-input-pipe
  #:use-module (ice-9 popen)
;;; (ice-9 readline) requires `guix install guile-readline'. However read-line
;;; might be already in the (ice-9 popen)
  ;; #:use-module (ice-9 readline)
  #:use-module (ice-9 rdelim)
  ;; string-match
  #:use-module (ice-9 regex)
  ;; delete-duplicates
  #:use-module (srfi srfi-1)
  ;; #:use-module (guix build utils) ;; invoke - not needed
  #:use-module (ice-9 pretty-print)
)

(define m (module-name-for-logging))
(evaluating-module)

;;; Use function-implementations so that the code below is not evaluated every
;;; time some of the scm-bin CLI utility requiring this module is executed.
(define (hostname)
  (let* ((ret (exec "hostname")))
    (if (= 0 (car ret))
        (let* [(output (cdr ret))
               (hostname (car output))]
          hostname)
        (begin
          (error-command-failed m)
          *unspecified*))))
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
