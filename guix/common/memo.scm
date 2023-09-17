(define-module (memo)
  #:use-module (settings)
  #:use-module (utils)

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
  #:export (
            hostname-memoized

            is-system-lukas
            is-system-ecke
            is-system-geek
            ))

;;; Use function-implementations so that the code below is not evaluated every
;;; time some of the scm-bin CLI utility requiring this module is executed.
(define (hostname)
  (let* ((ret (exec "hostname")))
    (if (= 0 (car ret))
        (let* [(output (cdr ret))
               (hostname (car output))]
          hostname)
        (begin
          (format #t "~a\n" (error-command-failed))
          *unspecified*))))
;; (format #t "[utils] hostname: ~a\n" hostname)

(define hostname-memoized (memoize hostname))

(define (is-system-lukas) (equal? (hostname-memoized) host-lukas))
(testsymb 'is-system-lukas)

(define (is-system-ecke) (equal? (hostname-memoized) host-ecke))
(testsymb 'is-system-ecke)

(define (is-system-geek) (equal? (hostname-memoized) host-geek))
(testsymb 'is-system-geek)