(define-module (profile-manifest)
  #:use-module (utils)            ; partial
  #:use-module (guix profiles)
  #:use-module (cfg packages all) ; home-packages-to-install
  )

(define m (module-name-for-logging))
(evaluating-module)

(define my-package? (@(guix packages) package?))
(define my-package-name (@(guix packages) package-name))
(define my-package->manifest-entry (@(guix profiles) package->manifest-entry))

(define my-inferior-package? (@(guix inferior) inferior-package?))
(define my-inferior-package-name (@(guix inferior) inferior-package-name))
(define my-inferior-package->manifest-entry (@(guix inferior) inferior-package->manifest-entry))

(define* (dbg-packages-to-install #:key search-space pkgs)
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
           (format #t "D Object is a list: ~a\n" p))]
        [(string? p)
         (when (member (my-package-name p) search-space)
           (format #t "D Object is a string: ~a\n" p))]
        [(my-package? p)
         (when (member (my-package-name p) search-space)
           (format #t "D Object is a package: ~a\n" p))]
        [(record? p)
         (when (member (my-inferior-package-name p) search-space)
           (format #t "D Object is a record: ~a\n" p))]
        [else
         (when (member (my-package-name p) search-space)
           (format #t "D Object is something else: ~a\n" p))])
       p))
    identity)
   pkgs)
  (format #t "I ~a Packages to install: ~a\n" m (length pkgs))
  pkgs)
(testsymb 'dbg-packages-to-install)

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

(define-public (manifest-content)
  "Leads to creation of a rather large file:
$ ls --human-readable --size /home/bost/.guix-profile/manifest
540K /home/bost/.guix-profile/manifest"
  (define f (format #f "~a [manifest-content]" m))
  ;; (format #t "~a Starting ...\n" f)
  ((comp
    ;; (lambda (p) (format #t "~a done.\n" f) p)
    manifest
    (partial map to-manifest-entry)
    ;; (lambda (p) (format #t "~a\n" ((@(ice-9 pretty-print) pretty-print) p)) p)
    ;; (lambda (p) ((@(srfi srfi-1) take) p 2))
    (partial dbg-packages-to-install #:search-space '() #:pkgs)
    ;; (lambda (p) (format #t "~a 0. (length p): ~a\n" f (length p)) p)
    )
   (home-packages-to-install))
  )
(testsymb 'manifest-content)

(module-evaluated)

(manifest-content)
