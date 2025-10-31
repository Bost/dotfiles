(define-module (kernel-version)
  #:use-module (dotf utils)         ; partial
  #:use-module (gnu packages)  ; find-packages-by-name
  #:use-module (guix packages) ; package-version
  ;; #:use-module (gnu system)    ; operating-system-kernel
  ;; #:use-module (dotf memo)          ; hostname-memoized
  ;; #:use-module (gnu tests)     ; %simple-os
 )

(define m (module-name-for-logging))
(evaluating-module)

((comp
  (partial apply (partial format #t "Kernel used: ~a. Latest available: ~a\n"))
  (lambda (kernel-version)
    (list kernel-version
          (package-version
           (car (find-packages-by-name "linux-libre"))
           ;; (operating-system-kernel %simple-os)
           ;; (operating-system-kernel
           ;;  (load
           ;;   (format #f "~a/guix/systems/syst-~a.scm"
           ;;           (getenv "dotf") (hostname-memoized))))
           ))))
;;; In the default Guix installation the ripgrep `rg` is not available and grep
;;; is build with --disable-perl-regexp, i.e. no -P --perl-regexp can be used.
 (let* [(cmd-result-struct
         (exec "uname -r | grep -o '\\([0-9]\\{1,\\}\\.\\)\\+[0-9]\\{1,\\}'"
               #:return-plist #t))
        (retcode (plist-get cmd-result-struct #:retcode))]
   (if (zero? retcode)
       (car (plist-get cmd-result-struct #:results))
       (begin
         (error (format #f "~a retcode: ~a\n" f retcode)) ; error-out
         ;; (error-command-failed f)
         ;; or return `retcode' instead of `*unspecified*'
         ;; *unspecified*
         ))))

(module-evaluated)
