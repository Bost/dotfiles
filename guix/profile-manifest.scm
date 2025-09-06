(define-module (profile-manifest)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (bost utils)
  #:use-module (bost tests)
  #:use-module (guix profiles)
  #:use-module (config packages all) ; home-packages-to-install
  )

(define m (module-name-for-logging))
(evaluating-module)

(define my-package? (@(guix packages) package?))
(define my-package-name (@(guix packages) package-name))
(define my-package->manifest-entry (@(guix profiles) package->manifest-entry))

(define my-inferior-package? (@(guix inferior) inferior-package?))
(define my-inferior-package-name (@(guix inferior) inferior-package-name))
(define my-inferior-package->manifest-entry (@(guix inferior) inferior-package->manifest-entry))

(define* (debug-packages-to-install #:key search-space pkgs)

  (define f (format #f "~a [debug-packages-to-install]" m))

  ((comp
    (lambda (p)
      (unless (unspecified-or-empty-or-false? p)
        (format #t "D ~a Packages in the search-space: ~a :\n~a\n"
                m search-space p))
      p)
    (partial
     filter
     ;; see also `string=?', `eq?', `equal?', etc.
     ;; member uses `equal?'
     (lambda (p)
       (cond
        [(my-package? p) (member (my-package-name p) search-space)]
        [(list? p)       (member (my-package-name (car p)) search-space)]
        [(string? p)     (member (my-package-name p) search-space)]
        [(record? p)     (member (my-inferior-package-name p) search-space)]
        [else            (member (my-package-name p) search-space)])))
    (partial
     map
     (lambda (p)
       (cond ;; we have a colorful mix here
        [(list? p)
         (when (member (my-package-name (car p)) search-space)
           (format #t "D ~a Object is a list: ~a\n" m p))]
        [(string? p)
         (when (member p search-space)
           (format #t "D ~a Object is a string: ~a\n" m p))]
        [(my-package? p)
         (when (member (my-package-name p) search-space)
           (format #t "D ~a Object is a package: ~a\n" m p))]
        [(record? p)
         (when (member (my-inferior-package-name p) search-space)
           (format #t "D ~a Object is a record: ~a\n" m p))]
        [else
         (when (member (my-package-name p) search-space)
           (format #t "D ~a Object is something else: ~a\n" m p))])
       p))
    identity)
   pkgs)
  (format #t "I ~a Packages to install: ~a\n"
          ;; Printing module-name is enough (m instead of f)
          m (length pkgs))
  pkgs)
(testsymb 'debug-packages-to-install)

(define-public (to-manifest-entry package)
  (cond
   [(list? package)
    (apply my-package->manifest-entry #;to-manifest-entry package)]
   [(my-inferior-package? package)
    (my-inferior-package->manifest-entry package)]
   ;; [(my-package? package)
   ;;  (my-package->manifest-entry package)]
   [else
    (my-package->manifest-entry package)]))
(testsymb 'to-manifest-entry)

(define (delete-duplicate-packages packages)
  (define f (format #f "~a [delete-duplicate-packages]" m))
  ((comp
    (lambda (lst)
      (let [(duplicates (find-duplicates lst string=?))]
        (if (not (empty? duplicates))
            (begin
              (my=warn "~a Removing ~a duplicate package(s):\n~a\n"
                       ;; Printing module-name is enough (m instead of f)
                       m (length duplicates) duplicates)
              (delete-duplicates packages))
            packages)))
    (partial map (lambda (p)
                   (cond ;; we have a colorful mix here
                    [(list? p)
                     (my-package-name (car p))]
                    [(string? p)
                     p]
                    [(my-package? p)
                     (my-package-name p)]
                    [(record? p)
                     (my-inferior-package-name p)]
                    [else
                     (my-package-name p)]))))
   packages))

(define-public (manifest-content)
  "Leads to creation of a rather large file:
$ ls --human-readable --size /home/bost/.guix-profile/manifest
540K /home/bost/.guix-profile/manifest"

  (define f (format #f "~a [manifest-content]" m))

  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (p) (format #t "~a done\n" f) p)
    manifest
    ;; (lambda (p) (format #t "~a 3. (length p): ~a\n" f (length p)) p)
    (partial map to-manifest-entry)
    ;; (lambda (p) (format #t "~a 2. ~a\n" f (pretty-print p)) p)
    ;; (lambda (p) (format #t "~a 2. (length p): ~a\n" f (length p)) p)
    ;; (lambda (p) (take p 2))
    (partial debug-packages-to-install #:search-space '() #:pkgs)
    delete-duplicate-packages
    ;; (lambda (p) (take p 2))
    ;; (lambda (p) (format #t "~a 1. (length p): ~a\n" f (length p)) p)
    ;; (partial debug-packages-to-install #:search-space '() #:pkgs)
    ;; (lambda (p) (format #t "~a 0. (length p): ~a\n" f (length p)) p)
    )
   (home-packages-to-install))
  )
(testsymb 'manifest-content)

(module-evaluated)

(manifest-content)
