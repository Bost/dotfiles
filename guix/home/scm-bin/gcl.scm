(define-module (scm-bin gcl)
  #:use-module (utils)
  #:export (main gcl))

#|

#!/usr/bin/env -S guile \\
-L ./ -e (gcl) -s
!#

|#

(define* (gcl #:rest args)
  "Usage:
(gcl \"-f\" \"arg0\")
(gcl \"-f arg0\")
(equal? (gcl \"-f\" \"arg0\")
        (gcl \"-f arg0\"))
;; > #t
"
  ;; git-command implementation see $dev/guix/tests/git.scm
  (apply exec-system*
         "git" "clone"
         args))

(define (show-help)
  (format #t "Usage: ~a [OPTION] NAME [ARGS ...]
Clone git repository using the following command:

  git clone [TODO ...]"
          (car (command-line)))
  (display "

Usage:
(main \"<ignored>\" \"-f\" \"arg0\")

Options:
  -h, --help        display this help and exit")
  (newline))


;; (use-modules (guix gexp))
;; (define remote "http://example.org/foo.git")
;; (define path "/tmp/")

;; (let* ((port ((@ (guix build utils) open-pipe-with-stderr)
;; 		          #$(file-append git "/bin/git") "clone" remote path)))
;; 	(waitpid WAIT_ANY)
;; 	(display ((@ (ice-9 rdelim) read-delimited) "" port))
;; 	(close-port port))

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
  (
   (compose
    (partial apply gcl)
    (partial apply cdr)
    #;dbg)
   #;
   (compose
    (lambda (cdr-args)
      (match (cdr-args)
        (((or "-h" "--help" "help"))
         (show-help))

        (#t (gcl cdr-args))
        ))
    (partial apply cdr)
    #;dbg)
   args)
  )
