(define-module (kernel-version)
  #:use-module (utils)         ; partial
  #:use-module (gnu packages)  ; find-packages-by-name
  #:use-module (guix packages) ; package-version
  ;; #:use-module (gnu system)    ; operating-system-kernel
  ;; #:use-module (memo)          ; hostname-memoized
  ;; #:use-module (gnu tests)     ; %simple-os
 )

(define m (module-name-for-logging))
(evaluating-module)

((comp
  (partial apply (partial format #t
                          (str "Kernel used: ~a. "
                               "Latest available: ~a\n"))))
 (list (let* ((ret
               (exec
 ;;; In the default Guix installation the ripgrep `rg` is not available and grep
 ;;; is build with --disable-perl-regexp, i.e. no -P --perl-regexp can be used.
                "uname -r | grep -o '\\([0-9]\\{1,\\}\\.\\)\\+[0-9]\\{1,\\}'")))
         (if (= 0 (car ret))
             (let* ((output (cdr ret)))
               (car output))
             (begin
               (error-command-failed m)
               *unspecified*)))
       (package-version
        (car (find-packages-by-name "linux-libre"))
        #;(operating-system-kernel %simple-os)
        #;
        (operating-system-kernel
        (load
         (format #f "~a/guix/systems/syst-~a.scm"
                 (getenv "dotf") (hostname-memoized)))))))

(module-evaluated)
