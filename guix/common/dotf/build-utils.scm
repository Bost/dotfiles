(define-module (dotf build-utils)
  #:use-module (guix store)
  #:use-module (guix packages) ; package?
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (ice-9 match)
  #:use-module (guix build utils) ; mkdir-p copy-recursively
  #:use-module (srfi srfi-26)  ; special selected function parameters
  #:use-module (ice-9 optargs) ; define*-public
  )

(define-public (build package)
  "
(build (@(bost gnu packages emacs-xyz) emacs-tweaks))

(build (@(gnu packages emacs-xyz) emacs-back-button)) ;=> #t
(build \"emacs-back-button\")                         ;=> #t
(build 'emacs-back-button)                            ;=> #t
"
  (define (->package p)
    (cond
     [(package? p) p]
     [(symbol? p) (specification->package (symbol->string p))]
     [(string? p) (specification->package p)]
     [else (error "Expected package, symbol, or string" p)]))

  (with-store store
    ((compose
      (cut build-derivations store <>)
      list
      (cut package-derivation store <>)
      ->package)
     package)))

(define-public (build-file-like-object obj)
  "
(build-file-like-object (program-file \"hello\" #~(display \"Hi!\")))
(build-file-like-object (plain-file \"hosts\" \"127.0.0.1 localhost\"))
"
  (define (store-item? obj)
    ;; Same idea as the local `store-item?` in `lower-inputs`
    ;; from (guix gexp): a store item is represented by its
    ;; /gnu/store/... file name.
    (and (string? obj)
         (store-path? obj)))

  (with-store store
    (run-with-store store
      (mlet %store-monad ((lowered (lower-object obj)))
        (match lowered
          ;; Things like `program-file`, `computed-file`, etc.
          ((? derivation? drv)
           (mbegin %store-monad
             (built-derivations (list drv))
             (return (derivation->output-path drv))))

          ;; Things like `plain-file` / sometimes `local-file`.
          ((? store-item? item)
           (return item)))))))

(define*-public (install-recursively source destination
                                     #:key
                                     (log (current-output-port))
                                     (follow-symlinks? #f)
                                     (copy-file copy-file)
                                     keep-mtime? keep-permissions?)
  "Recursive version of install-file."
  (mkdir-p destination)
  (copy-recursively
   source
   (string-append destination "/" (basename destination))
   #:log log
   #:follow-symlinks? follow-symlinks?
   #:copy-file copy-file
   #:keep-mtime? keep-mtime?
   #:keep-permissions? keep-permissions?
   ))

