#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (services\ guix-package) -s
!#

(define-module (services guix-package)
  #:use-module (utils)
  #:use-module (srfi-1-smart)
  #:use-module (tests)
  #:use-module (fs-utils)
  #:use-module (settings)
  #:use-module (cli-common)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (
            guix-package
            guix-list-installed
            guix-install
            guix-remove
            guix-search
            ))

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-package) -s
!#

cd $dotf
./guix/home/common/services/guix-package.scm --list-installed=emacs-helm-o

|#

(define m (module-name-for-logging))
(evaluating-module)

(define cores (or (getenv "cores") "1"))

(define (file-exists-and-readable? path)
  "Check if file exists and is readable"
  (access? path R_OK))

(define (get-extra-profiles)
  "Get extra profiles from GUIX_EXTRA_PROFILES directory"
  (define f (format #f "~a [get-extra-profiles]" m))
  (let [(extra-dir (getenv "GUIX_EXTRA_PROFILES"))]
    (if (and extra-dir (file-exists? extra-dir))
        ((comp
          (partial filter-map
                   (lambda (entry)
                     (let* [(profile-path (str extra-dir "/" entry "/" entry))
                            (profile (str profile-path "/etc/profile"))]
                       (if (file-exists-and-readable? profile)
                           profile
                           #f))))
          (lambda (dir) (scandir dir (lambda (entry)
                                       (not (member entry '("." ".."))))))
          (lambda (dir) (if (file-exists? dir) dir #f)))
         extra-dir)
        '())))

(define (get-all-profiles)
  "Collect all profiles from various sources"
  (define f (format #f "~a [get-all-profiles]" m))
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (v) (format #t "~a done\n" f) v)
    (lambda (profiles) (delete-duplicates profiles string=?))
    (lambda (base)
      (append base
              (list (user-home "/.guix-home/profile")
                    "/run/current-system/profile")
              (get-extra-profiles)))
    cdr
    exec)
   (str "guix package --cores=" cores " --list-profiles")))

(define (profile-param profile) (format #f "--profile=~a" profile))

(define (parse-package-output output)
  "Parse guix package output into structured data"
  (define f (format #f "~a [parse-package-output]" m))
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (v) (format #t "~a done\n" f) v)
    (partial map (lambda (parts)
                   `((name . ,(first parts))
                     (version . ,(second parts))
                     (path . ,(fourth parts)))))
    (partial map string-tokenize))
   output))

(define (string-pad-right str-to-pad width)
  "Helper function to pad strings for alignment"
  (let ((padding (- width (string-length str-to-pad))))
    (if (> padding 0)
        (str str-to-pad (make-string padding #\space))
        str-to-pad)))

(define (print-package-table packages)
  (map
   (lambda (pkg)
     (let ((name (assq-ref pkg 'name))
           (version (assq-ref pkg 'version))
           (path (assq-ref pkg 'path)))
       (format #t "    ~a ~a ~a\n"
               (string-pad-right name 25)
               (string-pad-right version 20)
               path)))
   packages))

;; (define p 'undef)
;; (define a 'undef)

(define* (guix-package options
;;; PROFILES being a list containing a single(!) empty(!) string means '... with
;;; default profile'. N empty strings would cause N (repeated) executions
                       #:key (profiles (list ""))
                       #:rest args)
  "Wrapper around `guix package ...'
Usage:
(guix-package \"--list-installed=emacs-helm-org\")"
  (define f (format #f "~a [guix-package]" m))
  ;; (format #t "~a Starting…\n" f)
  ;; (format #t "profiles : ~a\n" profiles)
  ;; (format #t "args : ~a\n" args)
  (let* [(elements (list #:profiles))
         (args (remove-all-elements args elements))
         (options (if (pair? options) (string-join options) options))]
    ((comp
      ;; (lambda (v) (format #t "~a done\n" f) v)
      (partial map (comp print-package-table parse-package-output cdr exec))
      (partial map (partial format #f "guix package --cores=~a ~a" cores))
      (partial map (comp
                    (lambda (lst) (string-join lst " "))
                    (lambda (lst) (append lst args))
                    (partial remove unspecified-or-empty-or-false?)
                    (partial list options)))
      #;(lambda (v) (format #t "~a 0:\n~a\n" m v) v))
     profiles)))
(testsymb 'guix-package)

(define* (guix-list-installed regexp)
  "Wrapper around `guix package --list-installed=...'
Usage:
(guix-list-installed \"emacs-helm-org\")"
  (guix-package
   (str "--list-installed=" regexp)
   #:profiles
   (map profile-param
        (get-all-profiles)
        ;; (list "/run/current-system/profile")
        )))
(testsymb 'guix-list-installed)

;; Options
(define --search "--search")
(define --install "--install")
(define --remove "--remove")

(define* (guix-package-alias alias options packages)
  "Wrapper around `guix package --package-alias ...'
Usage:
(guix-package-alias \"--install\" \"--dry-run\" \"emacs\")
(guix-package-alias \"--remove\" \"--dry-run\" \"emacs\")
(guix-package-alias \"--dry-run\" (list \"emacs\" \"coreutils\"))
(guix-package-alias (list \"--dry-run\" \"--fallback\") \"coreutils\")
"
  (define f (format #f "~a [guix-package-alias]" m))
  ;; (format #t "~a Starting…\n" f)
  (let* [(packages (if (pair? packages) (string-join packages) packages))
         (options (if (pair? options) (string-join options) options))]
    (guix-package options
                  (if (string= alias --search)
                      (format #f "~a=~a" alias packages)
                      (format #f "~a ~a" alias packages)))))

(define* (guix-install options packages)
  "Wrapper around `guix package --install ...'
Usage:
(guix-install \"--dry-run\" \"emacs\")
(guix-install \"--dry-run\" (list \"emacs\" \"coreutils\"))
(guix-install (list \"--dry-run\" \"--fallback\") \"coreutils\")
"
  (define f (format #f "~a [guix-install]" m))
  ;; (format #t "~a Starting…\n" f)
  (guix-package-alias --install options packages))
(testsymb 'guix-install)

(define* (guix-remove options packages)
  "Wrapper around `guix package --remove ...'
Usage:
(guix-remove \"--dry-run\" \"emacs\")
(guix-remove \"--dry-run\" (list \"emacs\" \"coreutils\"))
(guix-remove (list \"--dry-run\" \"--fallback\") \"coreutils\")
"
  (define f (format #f "~a [guix-remove]" m))
  ;; (format #t "~a Starting…\n" f)
  (guix-package-alias --remove options packages))
(testsymb 'guix-remove)

(define* (guix-search options packages)
  "Wrapper around `guix package --search ...'
Usage:
(guix-search \"--dry-run\" \"emacs-helm-org-contacts\")
(guix-search \"--dry-run\" (list \"emacs-helm-org-contacts \"coreutils\"))
(guix-search (list \"--dry-run\" \"--fallback\") \"coreutils\")
"
  (define f (format #f "~a [guix-search]" m))
  ;; (format #t "~a Starting…\n" f)
  (guix-package-alias --search options packages))
(testsymb 'guix-search)

(define-public (main args) (guix-package (cdr args)))

(module-evaluated)

;; In Emacs, see input history using: (comint-dynamic-list-input-ring)


