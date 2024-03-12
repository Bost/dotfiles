(define-module (manifest)
 #:use-module (guix profiles)
 #:use-module (cfg packages all))

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define-public (manifest-content)
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

#|
guix package \
  -L /home/bost/dev/dotfiles/guix/common \
  -L /home/bost/dev/dotfiles/guix/home/common \
  --profile=/home/bost/.guix-profile \
  --manifest=/home/bost/dev/dotfiles/guix/home/common/manifest.scm

(manifest-content)
|#

