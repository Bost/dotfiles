(use-modules
 (utils)         ; partial
 (gnu packages)  ; find-packages-by-name
 (guix packages) ; package-version
 ;; (gnu system)    ; operating-system-kernel
 ;; (memo)          ; hostname-memoized
 ;; (gnu tests)     ; %simple-os
 )

((compose
  (partial apply (partial format #t
                          (str "Kernel used: ~a. "
                               "Latest available: ~a\n"))))
 (list (let* ((ret (exec
 ;;; -o --only-matching, -P --perl-regexp - grep capture group with \K
                    "uname -r | rg -oP '([0-9]{1,}\\.)+[0-9]{1,}'")))
         (if (= 0 (car ret))
             (let* ((output (cdr ret)))
               (car output))
             (begin
               (format #t "~a\n" (error-command-failed))
               *unspecified*)))
       (package-version
        (car (find-packages-by-name "linux-libre"))
        #;(operating-system-kernel %simple-os)
        #;
        (operating-system-kernel
        (load
         (format #f "~a/guix/systems/syst-~a.scm"
                 (getenv "dotf") (hostname-memoized)))))))
