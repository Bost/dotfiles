(define-module (manifest)
  #:use-module (bost utils)
  #:use-module (guix profiles)
  #:use-module (cfg packages all))

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define-public (manifest-content)
"Leads to creation of a 540K large file:
$ ls --human-readable --size /home/bost/.guix-profile/manifest
540K /home/bost/.guix-profile/manifest"
  ((compose
    manifest
    (partial map
             (lambda (p)
               (cond
                [(list? p) (apply package->manifest-entry p)]
                [#t (package->manifest-entry p)])))
    #;(lambda (p) (format #t "~a\n" ((@(ice-9 pretty-print) pretty-print) p)) p)
    #;(lambda (p) ((@(srfi srfi-1) take) p 2)))
   (packages-to-install)))
(testsymb 'manifest-content)

(manifest-content)

