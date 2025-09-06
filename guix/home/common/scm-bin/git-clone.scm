(define-module (scm-bin git-clone)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (scm-bin git-command)
  #:export (main git-clone))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ git-clone) -s
!#

cd $dotf
./guix/home/common/scm-bin/git-clone.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (git-clone #:key (verbose #t) #:rest args)
  (apply (partial git-command "clone") args))

;; (use-modules (guix gexp))
;; (define remote "http://example.org/foo.git")
;; (define path "/tmp/")

;; (let* ((port ((@ (guix build utils) open-pipe-with-stderr)
;;              #$(file-append git "/bin/git") "clone" remote path)))
;;  (waitpid WAIT_ANY)
;;  (display ((@ (ice-9 rdelim) read-delimited) "" port))
;;  (close-port port))

;; (use-modules (git)
;;              ;; ((guix git) #:select (with-repository))

;; ;;; provides `git-command' which is a dynamically bound variable created by
;; ;;; make-parameter
;; ;;; https://www.gnu.org/software/guile/manual/html_node/Parameters.html
;;              (guix tests git)
;;              (guix utils)
;;              (guix build utils))

;; (define directory (format #f "~a/rde-andrew-tropin" (getenv "dev")))

;; (define (git command . args)
;;   ;; Make sure Git doesn't rely on the user's config.
;;   (call-with-temporary-directory
;;    (lambda (home)
;;      (call-with-output-file
;;          ;; The behavior of call-with-output-file is unspecified if the file already exists.
;;          (string-append home "/.gitconfig")
;;        (lambda (port)
;;          (display
;;           (string-append "[user]\n"
;;                          "  email = charlie@example.org\n"
;;                          "  name = Charlie Guix\n")
;;           port)))

;;      (with-environment-variables
;;          `(
;;            ;; ("GIT_CONFIG_NOSYSTEM" "1")
;;            ;; ("GIT_ATTR_NOSYSTEM" "1")
;;            ("GIT_CONFIG_GLOBAL" ,(string-append home "/.gitconfig"))
;;            ;; ("HOME" ,home)
;;            )
;;        (apply invoke (git-command) "-C" directory
;;               command args)))))
;; (git "config" "--global" "--get" "user.name") ;; => Charlie Guix
;; (git "status" "--short" "--branch")

;; https://gitlab.com/alezost-config/guile/-/blob/master/scripts/profile
(define* (main #:rest args)
  ((comp
    (partial apply git-clone)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
