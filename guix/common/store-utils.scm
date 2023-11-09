(define-module (store-utils)
  #:use-module (utils)
  #:use-module (guix)
  #:use-module (guix store)
)

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define-public (package-derivation-output! package)
  "
(package-derivation-output gcc-toolchain)
=> \"/gnu/store/fzsz6gk7g5spr7j5jx5zh6rysd5r0n64-gcc-toolchain-11.3.0\"

With side-effects. It modifies the /gnu/store. Moving this function to the
`utils' module leads to an error, since the `utils' module is used in the
scripts. E.g. './scm-bin/gtg.scm'

In the REPL invoke:
  (use-modules (gnu packages commencement))
  (use-modules (guix) (guix store))
"
  (define package-drv
    (with-store %store
      (run-with-store %store
        (lower-object package))))

  (with-store %store (build-derivations %store (list package-drv)))

  (define package-drv-out
    (derivation-output-path
     (assoc-ref (derivation-outputs package-drv) "out")))

  ;; The output directory structure:
  ;; ((@(ice-9 ftw) scandir) gcct-drv-out)
  ;; => ("." ".." "bin" "etc" "include" "lib" "libexec" "sbin" "share" "var" "x86_64-unknown-linux-gnu")
  package-drv-out)

;; (format #t "~a module evaluated\n" m)
